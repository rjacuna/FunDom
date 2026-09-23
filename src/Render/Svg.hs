-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | The picture, as SVG. Knows nothing about groups: it is handed shapes,
-- colours and hrefs.
--
-- It is built as a 'Builder' of UTF-8 bytes rather than a 'String'. The disk
-- tilings run to a megabyte and more of path data, and a 'String' is a list
-- of boxed characters: building one, and then encoding it for the browser,
-- cost about a second and a half per megabyte in the wasm build, which is
-- most of what the page took. A builder writes into buffers instead.
{-# LANGUAGE OverloadedStrings #-}
module Render.Svg
  ( Scene(..), Tri(..), Dot(..), Layer(..), uhpScene, renderSvg, fmt, fmtB, escB, str
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Char8 ()          -- IsString, so a literal below is bytes already
import Data.Monoid (mconcat)
import Data.Ratio
import Flint.Ball
import Modular.Geometry
import Numeric (showFFloat)

data Tri = Tri
  { tShape  :: Shape
  , tFill   :: String
  , tStroke :: String
  , tWidth  :: Double
  , tHref   :: Maybe String
  , tTitle  :: String
  }

-- | A batch of shapes drawn under the triangles, in one style: the
-- tessellation, the Γ-translates, their boundaries.
data Layer = Layer
  { lyFill    :: String
  , lyStroke  :: String
  , lyWidth   :: Double
  , lyOpacity :: Double
  , lyClosed  :: Bool
  , lyShapes  :: [Shape]
  }

data Dot = Dot
  { dPos   :: Screen
  , dHref  :: Maybe String   -- ^ a move to make; 'Nothing' marks a side paired with itself
  , dTitle :: String
  }

data Scene = Scene
  { scView    :: View
  , scTris    :: [Tri]
  , scOpacity :: Double             -- ^ fill opacity of the triangles
  , scLinks   :: [(Screen, Screen)] -- ^ curves between paired sides
  , scDots    :: [Dot]              -- ^ side-pairing markers, in edit mode
  , scCusps   :: [(Double, String)] -- ^ screen x and label
  , scNote    :: String             -- ^ a line of text in the top-left corner
  , scAxis    :: Bool               -- ^ the real axis with ticks (half-plane)
  , scDisk    :: Maybe (DiskView, String)  -- ^ the unit circle, filled with a colour (disk)
  , scLayers  :: [Layer]            -- ^ drawn under the triangles, in order
  }

-- | A half-plane scene with nothing underneath.
uhpScene :: View -> [Tri] -> [(Screen, Screen)] -> [Dot] -> [(Double, String)] -> String -> Scene
uhpScene v tris links dots cusps note = Scene v tris 0.85 links dots cusps note True Nothing []

fmt :: Double -> String
fmt x
  | isNaN x || isInfinite x = "0"
  | otherwise = showFFloat (Just 2) (max (-1.0e9) (min 1.0e9 x)) ""

-- | Two decimals, as 'fmt', without going through a 'String': this is the
-- hot one, a dozen numbers for every shape and tens of thousands of shapes.
fmtB :: Double -> Builder
fmtB x
  | isNaN x || isInfinite x = char8 '0'
  | otherwise =
      let n        = round (max (-1.0e9) (min 1.0e9 x) * 100) :: Int
          (q, r)   = abs n `quotRem` 100
          sign     = if n < 0 then char8 '-' else mempty
          decimals = if r < 10 then char8 '0' <> intDec r else intDec r
      in sign <> intDec q <> char8 '.' <> decimals

-- | A literal of the markup. Bytes, not a 'String': with OverloadedStrings
-- every literal below is a 'ByteString' known at compile time, and writing it
-- is a copy rather than a walk down a list of characters.
str :: ByteString -> Builder
str = byteString

-- | Text from outside: escaped, and encoded as UTF-8, labels being what they are.
escB :: String -> Builder
escB = mconcat . map e
  where
    e '<' = str "&lt;"
    e '>' = str "&gt;"
    e '&' = str "&amp;"
    e '"' = str "&quot;"
    e ch | ch < '\128' = char8 ch
         | otherwise   = charUtf8 ch

ptB :: Screen -> Builder
ptB (x, y) = fmtB x <> char8 ' ' <> fmtB y

pathD :: Shape -> Builder
pathD sh = pathOpen sh <> str " Z"

pathOpen :: Shape -> Builder
pathOpen sh = char8 'M' <> ptB (shStart sh) <> mconcat (map seg (shSegs sh))
  where
    seg (ArcTo r sw p) = str " A" <> fmtB r <> char8 ' ' <> fmtB r <> str (if sw then " 0 0 1 " else " 0 0 0 ") <> ptB p
    seg (LineTo p)     = str " L" <> ptB p

renderSvg :: Scene -> Builder
renderSvg sc = nl $
  [ str "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"" <> intDec w <> str "\" height=\"" <> intDec h
      <> str "\" viewBox=\"0 0 " <> intDec w <> char8 ' ' <> intDec h <> str "\" style=\"background:#fff;display:block\">"
  , str "<rect x=\"0\" y=\"0\" width=\"" <> intDec w <> str "\" height=\"" <> intDec h <> str "\" fill=\"#fff\"/>" ] ++
  [ str "<circle cx=\"" <> fmtB (dvCx dv) <> str "\" cy=\"" <> fmtB (dvCy dv) <> str "\" r=\"" <> fmtB (dvR dv) <> str "\" fill=\"" <> escB c <> str "\" stroke=\"none\"/>"
  | Just (dv, c) <- [scDisk sc] ] ++
  [ str "<clipPath id=\"disk\"><circle cx=\"" <> fmtB (dvCx dv) <> str "\" cy=\"" <> fmtB (dvCy dv) <> str "\" r=\"" <> fmtB (dvR dv) <> str "\"/></clipPath>"
  | Just (dv, _) <- [scDisk sc] ] ++
  [ str "<g id=\"layers\"" <> (case scDisk sc of { Just _ -> str " clip-path=\"url(#disk)\""; Nothing -> mempty }) <> char8 '>' ] ++
  concatMap layer (scLayers sc) ++
  [ str "</g>", str "<g id=\"tris\">" ] ++
  map tri (scTris sc) ++
  [ str "</g>" ] ++
  (if scAxis sc then
     [ str "<g id=\"axis\" stroke=\"#333\" stroke-width=\"1\" font-family=\"sans-serif\" font-size=\"11\" fill=\"#333\">"
     , str "<line x1=\"0\" y1=\"" <> fmtB ay <> str "\" x2=\"" <> intDec w <> str "\" y2=\"" <> fmtB ay <> str "\"/>" ] ++
     concatMap tick (ticks v) ++ [ str "</g>" ]
   else []) ++
  [ str "<circle cx=\"" <> fmtB (dvCx dv) <> str "\" cy=\"" <> fmtB (dvCy dv) <> str "\" r=\"" <> fmtB (dvR dv) <> str "\" fill=\"none\" stroke=\"#333\" stroke-width=\"1\"/>"
  | Just (dv, _) <- [scDisk sc] ] ++
  [ str "<g id=\"cusps\" font-family=\"sans-serif\" font-size=\"10\" fill=\"#0033aa\" stroke=\"#0033aa\">" ] ++
  concatMap cuspMark (scCusps sc) ++
  [ str "</g>", str "<g id=\"links\" fill=\"none\" stroke=\"#0a7\" stroke-width=\"1.2\">" ] ++
  map link (scLinks sc) ++
  [ str "</g>", str "<g id=\"dots\">" ] ++
  map dot (scDots sc) ++
  [ str "</g>"
  , str "<text x=\"8\" y=\"16\" font-family=\"sans-serif\" font-size=\"12\" fill=\"#444\">" <> escB (scNote sc) <> str "</text>"
  , str "</svg>" ]
  where
    v  = scView sc
    w  = vW v
    h  = vH v
    ay = axisY v

    -- the lines of the file, as `intercalate "\n"` made them
    nl []       = mempty
    nl (b : bs) = b <> mconcat [ char8 '\n' <> b' | b' <- bs ]

    layer ly =
      [ str "<g fill=\"" <> escB (lyFill ly) <> str "\" stroke=\"" <> escB (lyStroke ly) <> str "\" stroke-width=\"" <> fmtB (lyWidth ly)
        <> str "\" fill-opacity=\"" <> fmtB (lyOpacity ly) <> str "\" stroke-linejoin=\"round\" stroke-linecap=\"round\">" ] ++
      [ str "<path d=\"" <> (if lyClosed ly then pathD sh else pathOpen sh) <> str "\"/>" | sh <- lyShapes ly ] ++
      [ str "</g>" ]

    tri t =
      let body = str "<path d=\"" <> pathD (tShape t) <> str "\" fill=\"" <> escB (tFill t) <> str "\" stroke=\"" <> escB (tStroke t)
                 <> str "\" stroke-width=\"" <> fmtB (tWidth t) <> str "\" stroke-linejoin=\"round\" fill-opacity=\"" <> fmtB (scOpacity sc) <> str "\">"
                 <> str "<title>" <> escB (tTitle t) <> str "</title></path>"
      in case tHref t of
           Just u  -> str "<a href=\"" <> escB u <> str "\">" <> body <> str "</a>"
           Nothing -> body

    tick (x, label) =
      [ str "<line x1=\"" <> fmtB x <> str "\" y1=\"" <> fmtB ay <> str "\" x2=\"" <> fmtB x <> str "\" y2=\"" <> fmtB (ay + 5) <> str "\"/>"
      , str "<text x=\"" <> fmtB x <> str "\" y=\"" <> fmtB (ay + 17) <> str "\" text-anchor=\"middle\" stroke=\"none\">" <> escB label <> str "</text>" ]

    cuspMark (x, label)
      | x < -20 || x > fromIntegral w + 20 = []
      | otherwise =
          [ str "<line x1=\"" <> fmtB x <> str "\" y1=\"" <> fmtB (ay - 4) <> str "\" x2=\"" <> fmtB x <> str "\" y2=\"" <> fmtB (ay + 4) <> str "\" stroke-width=\"1.5\"/>"
          , str "<text x=\"" <> fmtB x <> str "\" y=\"" <> fmtB (ay + 30) <> str "\" text-anchor=\"middle\" stroke=\"none\">" <> escB label <> str "</text>" ]

    -- a quadratic Bézier whose apex sits above the chord by an eighth of its length
    link ((x1, y1), (x2, y2)) =
      let mx = (x1 + x2) / 2
          my = min y1 y2 - abs (x1 - x2) / 8 - 6
          cx = 2 * mx - (x1 + x2) / 2
          cy = 2 * my - (y1 + y2) / 2
      in str "<path d=\"M" <> ptB (x1, y1) <> str " Q" <> ptB (cx, cy) <> char8 ' ' <> ptB (x2, y2) <> str "\"/>"

    dot d =
      let (x, y) = dPos d
          circle r fillc = str "<circle cx=\"" <> fmtB x <> str "\" cy=\"" <> fmtB y <> str "\" r=\"" <> str r <> str "\" fill=\"" <> str fillc
                           <> str "\" stroke=\"#000\" stroke-width=\"1\"><title>" <> escB (dTitle d) <> str "</title></circle>"
      in case dHref d of
           Just u  -> str "<a href=\"" <> escB u <> str "\">" <> circle "5" "#ffd700" <> str "</a>"
           Nothing -> circle "3.5" "#d0d0d0"

-- | Axis ticks at a round step wide enough to keep labels apart.
ticks :: View -> [(Double, String)]
ticks v = [ (projectX v x, label x) | k <- [k0 .. k1], let x = fromInteger k * step ]
  where
    sc    = vScale v
    -- the smallest of 1,2,5 × 10^e with step·scale ≥ 70 px
    step  = case [ s | e <- [-12 .. 12 :: Int], m <- [1, 2, 5], let s = m * (10 ^^ e), s * sc >= 70 ] of
              (s : _) -> s `asTypeOf` sc
              []      -> 10 ^^ (12 :: Int)
    half  = (fromIntegral (vW v) / 2) / sc
    lo    = vCx v - half
    hi    = vCx v + half
    k0    = ceiling (lo / step) :: Integer
    k1    = floor (hi / step)
    label x
      | denominator x == 1 = show (numerator x)
      | otherwise = showFFloat (Just (digits step)) (fromRational x :: Double) ""
    digits s = length (takeWhile (< 1) (iterate (* 10) s))
