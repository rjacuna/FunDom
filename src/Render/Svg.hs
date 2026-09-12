-- | The picture, as an SVG string. Knows nothing about groups: it is handed
-- shapes, colours and hrefs.
module Render.Svg
  ( Scene(..), Tri(..), Dot(..), Layer(..), uhpScene, renderSvg, fmt
  ) where

import Data.List (intercalate)
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

pt :: Screen -> String
pt (x, y) = fmt x ++ " " ++ fmt y

esc :: String -> String
esc = concatMap e
  where
    e '<' = "&lt;"
    e '>' = "&gt;"
    e '&' = "&amp;"
    e '"' = "&quot;"
    e ch  = [ch]

pathD :: Shape -> String
pathD sh = pathOpen sh ++ " Z"

pathOpen :: Shape -> String
pathOpen sh = "M" ++ pt (shStart sh) ++ concatMap seg (shSegs sh)
  where
    seg (ArcTo r sw p) = " A" ++ fmt r ++ " " ++ fmt r ++ " 0 0 " ++ (if sw then "1" else "0") ++ " " ++ pt p
    seg (LineTo p)     = " L" ++ pt p

renderSvg :: Scene -> String
renderSvg sc = intercalate "\n" $
  [ "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"" ++ show w ++ "\" height=\"" ++ show h
      ++ "\" viewBox=\"0 0 " ++ show w ++ " " ++ show h ++ "\" style=\"background:#fff;display:block\">"
  , "<rect x=\"0\" y=\"0\" width=\"" ++ show w ++ "\" height=\"" ++ show h ++ "\" fill=\"#fff\"/>" ] ++
  [ "<circle cx=\"" ++ fmt (dvCx dv) ++ "\" cy=\"" ++ fmt (dvCy dv) ++ "\" r=\"" ++ fmt (dvR dv) ++ "\" fill=\"" ++ esc c ++ "\" stroke=\"none\"/>"
  | Just (dv, c) <- [scDisk sc] ] ++
  [ "<clipPath id=\"disk\"><circle cx=\"" ++ fmt (dvCx dv) ++ "\" cy=\"" ++ fmt (dvCy dv) ++ "\" r=\"" ++ fmt (dvR dv) ++ "\"/></clipPath>"
  | Just (dv, _) <- [scDisk sc] ] ++
  [ "<g id=\"layers\"" ++ (case scDisk sc of { Just _ -> " clip-path=\"url(#disk)\""; Nothing -> "" }) ++ ">" ] ++
  concatMap layer (scLayers sc) ++
  [ "</g>", "<g id=\"tris\">" ] ++
  map tri (scTris sc) ++
  [ "</g>" ] ++
  (if scAxis sc then
     [ "<g id=\"axis\" stroke=\"#333\" stroke-width=\"1\" font-family=\"sans-serif\" font-size=\"11\" fill=\"#333\">"
     , "<line x1=\"0\" y1=\"" ++ fmt ay ++ "\" x2=\"" ++ show w ++ "\" y2=\"" ++ fmt ay ++ "\"/>" ] ++
     concatMap tick (ticks v) ++ [ "</g>" ]
   else []) ++
  [ "<circle cx=\"" ++ fmt (dvCx dv) ++ "\" cy=\"" ++ fmt (dvCy dv) ++ "\" r=\"" ++ fmt (dvR dv) ++ "\" fill=\"none\" stroke=\"#333\" stroke-width=\"1\"/>"
  | Just (dv, _) <- [scDisk sc] ] ++
  [ "<g id=\"cusps\" font-family=\"sans-serif\" font-size=\"10\" fill=\"#0033aa\" stroke=\"#0033aa\">" ] ++
  concatMap cuspMark (scCusps sc) ++
  [ "</g>", "<g id=\"links\" fill=\"none\" stroke=\"#0a7\" stroke-width=\"1.2\">" ] ++
  map link (scLinks sc) ++
  [ "</g>", "<g id=\"dots\">" ] ++
  map dot (scDots sc) ++
  [ "</g>"
  , "<text x=\"8\" y=\"16\" font-family=\"sans-serif\" font-size=\"12\" fill=\"#444\">" ++ esc (scNote sc) ++ "</text>"
  , "</svg>" ]
  where
    v  = scView sc
    w  = vW v
    h  = vH v
    ay = axisY v

    layer ly =
      [ "<g fill=\"" ++ esc (lyFill ly) ++ "\" stroke=\"" ++ esc (lyStroke ly) ++ "\" stroke-width=\"" ++ fmt (lyWidth ly)
        ++ "\" fill-opacity=\"" ++ fmt (lyOpacity ly) ++ "\" stroke-linejoin=\"round\" stroke-linecap=\"round\">" ] ++
      [ "<path d=\"" ++ (if lyClosed ly then pathD sh else pathOpen sh) ++ "\"/>" | sh <- lyShapes ly ] ++
      [ "</g>" ]

    tri t =
      let body = "<path d=\"" ++ pathD (tShape t) ++ "\" fill=\"" ++ esc (tFill t) ++ "\" stroke=\"" ++ esc (tStroke t)
                 ++ "\" stroke-width=\"" ++ fmt (tWidth t) ++ "\" stroke-linejoin=\"round\" fill-opacity=\"" ++ fmt (scOpacity sc) ++ "\">"
                 ++ "<title>" ++ esc (tTitle t) ++ "</title></path>"
      in case tHref t of
           Just u  -> "<a href=\"" ++ esc u ++ "\">" ++ body ++ "</a>"
           Nothing -> body

    tick (x, label) =
      [ "<line x1=\"" ++ fmt x ++ "\" y1=\"" ++ fmt ay ++ "\" x2=\"" ++ fmt x ++ "\" y2=\"" ++ fmt (ay + 5) ++ "\"/>"
      , "<text x=\"" ++ fmt x ++ "\" y=\"" ++ fmt (ay + 17) ++ "\" text-anchor=\"middle\" stroke=\"none\">" ++ esc label ++ "</text>" ]

    cuspMark (x, label)
      | x < -20 || x > fromIntegral w + 20 = []
      | otherwise =
          [ "<line x1=\"" ++ fmt x ++ "\" y1=\"" ++ fmt (ay - 4) ++ "\" x2=\"" ++ fmt x ++ "\" y2=\"" ++ fmt (ay + 4) ++ "\" stroke-width=\"1.5\"/>"
          , "<text x=\"" ++ fmt x ++ "\" y=\"" ++ fmt (ay + 30) ++ "\" text-anchor=\"middle\" stroke=\"none\">" ++ esc label ++ "</text>" ]

    -- a quadratic Bézier whose apex sits above the chord by an eighth of its length
    link ((x1, y1), (x2, y2)) =
      let mx = (x1 + x2) / 2
          my = min y1 y2 - abs (x1 - x2) / 8 - 6
          cx = 2 * mx - (x1 + x2) / 2
          cy = 2 * my - (y1 + y2) / 2
      in "<path d=\"M" ++ pt (x1, y1) ++ " Q" ++ pt (cx, cy) ++ " " ++ pt (x2, y2) ++ "\"/>"

    dot d =
      let (x, y) = dPos d
          circle r fillc = "<circle cx=\"" ++ fmt x ++ "\" cy=\"" ++ fmt y ++ "\" r=\"" ++ r ++ "\" fill=\"" ++ fillc
                           ++ "\" stroke=\"#000\" stroke-width=\"1\"><title>" ++ esc (dTitle d) ++ "</title></circle>"
      in case dHref d of
           Just u  -> "<a href=\"" ++ esc u ++ "\">" ++ circle "5" "#ffd700" ++ "</a>"
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
