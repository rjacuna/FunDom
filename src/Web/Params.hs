-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | The state of the page, which is the query string and nothing else.
--
-- Every control on the page is a link or a GET form, so a picture — group,
-- zoom, centre, colours, the side-pairings the user has rearranged — is a
-- URL, and the page is a pure function of it. That is what makes the
-- server trivial, and what will make the single-page version trivial too:
-- the same string goes into the same function.
module Web.Params
  ( Mode(..), ViewKind(..), Params(..), defaultParams
  , parseParams, parseQuery, urlDecode, urlEncode
  , toQuery, href
  , readRat, showRat
  , readMoves, showMoves, readMats, showMats
  , colourNames, colourCss
  ) where

import Data.Char (isDigit, isHexDigit, digitToInt, ord, isAlphaNum, toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ratio
import Numeric (showHex)
import Flint.SL2
import Modular.Domain (Move, genCode, genFromCode)
import Modular.Group

data Mode = DomainMode | TriMode deriving (Eq, Show)

-- | The upper half-plane, or the unit disk under the Cayley transform.
data ViewKind = UHP | Disk deriving (Eq, Show)

data Params = Params
  { pG1, pG2      :: GroupType
  , pN, pM        :: Int
  , pScale        :: Rational   -- ^ pixels per unit
  , pCx           :: Rational   -- ^ real part at the centre of the view
  , pEdit         :: Bool       -- ^ show side-pairing markers to click
  , pLinks        :: Bool       -- ^ draw curves between paired sides
  , pCuspLabels   :: Bool
  , pSel          :: Maybe Int  -- ^ highlighted triangle
  , pMoves        :: [Move]     -- ^ the user's rearrangements, in order
  , pFill         :: String
  , pOutline      :: String
  , pMode         :: Mode
  , pMats         :: [SL2]      -- ^ triangle-explorer mode: the matrices drawn
  , pCopy         :: Bool       -- ^ explorer: append rather than replace
  , pW, pH        :: Int
  , pAuto         :: Bool       -- ^ no zoom or centre given: fit the picture to the view
  , pDb           :: Maybe String  -- ^ a Cummins–Pauli name; overrides the classical selectors
  , pView         :: ViewKind
  , pBg           :: Bool       -- ^ disk: the two-coloured modular tessellation behind the domain
  , pTile         :: Bool       -- ^ disk: tile the disk with Γ-translates of the domain
  , pC1, pC2      :: String     -- ^ the two colours of the tessellation
  , pFill2        :: String     -- ^ the second colour of the Γ-tiling
  , pApp          :: Int        -- ^ 1 classical families, 2 the tables, 3 generators
  , pGenus, pLevel, pIndex :: Maybe Int   -- ^ mode 2: the filters chosen
  , pBy           :: Maybe String  -- ^ mode 2: the category whose dropdown is open next (gen, lev, idx)
  , pGens         :: String     -- ^ mode 3: the generator box
  } deriving (Show)

defaultParams :: Params
defaultParams = Params
  { pG1 = G0, pG2 = G0, pN = 1, pM = 1
  , pScale = 50, pCx = 0
  , pEdit = False, pLinks = False, pCuspLabels = True
  , pSel = Nothing, pMoves = []
  , pFill = "red", pOutline = "black"
  , pMode = DomainMode, pMats = [identity], pCopy = False
  , pW = 900, pH = 520
  , pAuto = False
  , pDb = Nothing, pView = UHP, pBg = True, pTile = False
  , pC1 = "white", pC2 = "grey", pFill2 = "babyblue"
  , pApp = 1, pGenus = Nothing, pLevel = Nothing, pIndex = Nothing, pBy = Nothing, pGens = ""
  }

-- Colours ----------------------------------------------------------------------

-- | The Java palette, plus a per-triangle rainbow and "none".
colourNames :: [String]
colourNames = map fst palette ++ ["rainbow", "none"]

palette :: [(String, String)]
palette =
  [ ("red", "#ff3b30"), ("grey", "#e6e6e6"), ("mauve", "#c80096"), ("babyblue", "#bebeff")
  , ("turquoise", "#46dcc8"), ("orange", "#fa9b0a"), ("apple", "#befa82"), ("banana", "#ffdc00")
  , ("chocolate", "#783232"), ("black", "#000000"), ("white", "#ffffff"), ("blue", "#1f5fff") ]

-- | CSS colour for the triangle with the given index.
colourCss :: String -> Int -> String
colourCss "rainbow" i = "hsl(" ++ show ((i * 137) `mod` 360) ++ ",70%,60%)"
colourCss "none"    _ = "none"
colourCss name      _ = fromMaybe "#ff3b30" (lookup name palette)

-- Query strings ----------------------------------------------------------------

parseQuery :: String -> [(String, String)]
parseQuery = mapMaybe pair . splitOn '&' . dropWhile (== '?')
  where
    pair "" = Nothing
    pair s  = let (k, v) = break (== '=') s in Just (urlDecode k, urlDecode (drop 1 v))

splitOn :: Char -> String -> [String]
splitOn ch s = case break (== ch) s of
  (w, [])    -> [w]
  (w, _ : r) -> w : splitOn ch r

urlDecode :: String -> String
urlDecode ('%' : a : b : r) | isHexDigit a && isHexDigit b = toEnum (16 * digitToInt a + digitToInt b) : urlDecode r
urlDecode ('+' : r) = ' ' : urlDecode r
urlDecode (ch : r)  = ch : urlDecode r
urlDecode []        = []

urlEncode :: String -> String
urlEncode = concatMap enc
  where
    enc ch | isAlphaNum ch && ord ch < 128 = [ch]
           | ch `elem` "-_.~,;:/" = [ch]
           | otherwise = concatMap hex (utf8 (ord ch))
    hex n = '%' : map toUpper ((if n < 16 then "0" else "") ++ showHex n "")
    utf8 n | n < 0x80    = [n]
           | n < 0x800   = [0xC0 + n `div` 64, 0x80 + n `mod` 64]
           | n < 0x10000 = [0xE0 + n `div` 4096, 0x80 + (n `div` 64) `mod` 64, 0x80 + n `mod` 64]
           | otherwise   = [0xF0 + n `div` 262144, 0x80 + (n `div` 4096) `mod` 64, 0x80 + (n `div` 64) `mod` 64, 0x80 + n `mod` 64]

-- Rationals in URLs and text boxes ----------------------------------------------

-- | Accepts @-12@, @3.25@, @-3/7@.
readRat :: String -> Maybe Rational
readRat s0 = case filter (/= ' ') s0 of
  '-' : s -> negate <$> pos s
  '+' : s -> pos s
  s       -> pos s
  where
    pos s | null s = Nothing
    pos s = case break (`elem` "/.") s of
      (n, [])        | all isDigit n && not (null n) -> Just (read n % 1)
      (n, '/' : dn)  | ok n && ok dn && read dn /= (0 :: Integer) -> Just (read n % read dn)
      (n, '.' : fr)  | all isDigit n && all isDigit fr && not (null n && null fr) ->
                         let n' = if null n then "0" else n
                             fr' = if null fr then "0" else fr
                         in Just ((read n' * 10 ^ length fr' + read fr') % (10 ^ length fr'))
      _ -> Nothing
    ok t = all isDigit t && not (null t)

-- | Decimal when it terminates within six places, otherwise a fraction.
showRat :: Rational -> String
showRat r
  | denominator r == 1 = show (numerator r)
  | 1000000 `mod` denominator r == 0 =
      let sgn = if r < 0 then "-" else ""
          ar = abs r
          whole = numerator ar `div` denominator ar
          frac = numerator (ar - fromInteger whole) * (1000000 `div` denominator ar)
          digits = reverse (dropWhile (== '0') (reverse (pad6 (show frac))))
      in sgn ++ show whole ++ "." ++ digits
  | otherwise = show (numerator r) ++ "/" ++ show (denominator r)
  where pad6 t = replicate (6 - length t) '0' ++ t

-- Moves and matrices --------------------------------------------------------------

showMoves :: [Move] -> String
showMoves ms = intercalate "," [ show i ++ [genCode g] | (i, g) <- ms ]

readMoves :: String -> [Move]
readMoves = mapMaybe one . splitOn ','
  where
    one t = case span isDigit t of
      (ds@(_ : _), [ch]) -> (,) (read ds) <$> genFromCode ch
      _                  -> Nothing

-- | Matrices are joined with @_@: the obvious @;@ is a query-string
-- separator to WAI's parser, and would be split before we saw it.
showMats :: [SL2] -> String
showMats ms = intercalate "_" [ intercalate "," (map show [p, q, r, s]) | m <- ms, let (p, q, r, s) = entries m ]

readMats :: String -> [SL2]
readMats = mapMaybe one . splitOn '_'
  where
    one t = case mapM readInt (splitOn ',' t) of
      Just [p, q, r, s] -> sl2 p q r s
      _                 -> Nothing
    readInt u = case u of
      '-' : ds | all isDigit ds && not (null ds) -> Just (negate (read ds))
      ds       | all isDigit ds && not (null ds) -> Just (read ds)
      _ -> Nothing

-- The record -----------------------------------------------------------------------

parseParams :: [(String, String)] -> Params
parseParams kv = Params
  { pG1 = tp "g1" (pG1 defaultParams)
  , pG2 = tp "g2" (pG2 defaultParams)
  , pN  = clamp 1 100000 (int "n" 1)
  , pM  = clamp 1 100000 (int "m" 1)
  , pScale = clampR (1 % 1048576) (2 ^ (40 :: Int)) (rat "scale" 50)
  , pCx    = rat "cx" 0
  , pEdit  = flag "edit" False
  , pLinks = flag "links" False
  , pCuspLabels = flag "cusps" True
  , pSel   = case get "sel" of
               Just s | all isDigit s && not (null s) -> Just (read s)
               _ -> Nothing
  , pMoves = maybe [] readMoves (get "mv")
  , pFill    = colour "fill" "red"
  , pOutline = colour "outline" "black"
  , pMode  = if get "mode" == Just "tri" then TriMode else DomainMode
  , pMats  = case maybe [] readMats (get "mats") ++ maybe [] (: []) newMat of
               [] -> [identity]
               ms -> take 200 ms
  , pCopy  = flag "copy" False
  , pW     = clamp 200 4000 (int "w" 900)
  , pH     = clamp 200 4000 (int "h" 520)
  , pAuto  = get "scale" == Nothing && get "cx" == Nothing
  , pDb    = case get "db" of
               Just t | not (null t), all (\ch -> isAlphaNum ch && ord ch < 128) t -> Just (map toUpper t)
               _ -> Nothing
  , pView  = if get "view" == Just "disk" then Disk else UHP
  , pBg    = flag "bg" True
  , pTile  = flag "tile" False
  , pC1    = colour "c1" "white"
  , pC2    = colour "c2" "grey"
  , pFill2 = colour "fill2" "babyblue"
  , pApp   = case get "app" of
               Just "2" -> 2
               Just "3" -> 3
               Just "1" -> 1
               _ | get "gens" /= Nothing -> 3
                 | get "db" /= Nothing || get "gen" /= Nothing -> 2
                 | otherwise -> 1
  , pGenus = nat "gen"
  , pLevel = nat "lev"
  , pIndex = nat "idx"
  , pBy    = case get "by" of
               Just t | t `elem` ["gen", "lev", "idx"] -> Just t
               _ -> Nothing
  , pGens  = maybe "" (take 4000) (get "gens")
  }
  where
    get k = lookup k kv
    nat k = case get k of
      Just t | all isDigit t && not (null t) -> Just (read t)
      _ -> Nothing
    -- the explorer's a b c d boxes: appended to the list if they multiply out
    newMat = do
      [p', q', r', s'] <- mapM (\k -> get k >>= readRat) ["a", "b", "c", "d"]
      if all ((== 1) . denominator) [p', q', r', s']
        then sl2 (numerator p') (numerator q') (numerator r') (numerator s')
        else Nothing
    tp k dflt = fromMaybe dflt (get k >>= typeFromCode)
    int k dflt = case get k of
      Just s -> case readRat s of
        Just r | denominator r == 1 -> fromInteger (numerator r)
        _ -> dflt
      Nothing -> dflt
    rat k dflt = fromMaybe dflt (get k >>= readRat)
    flag k dflt = case get k of
      Just "1" -> True
      Just "0" -> False
      _        -> dflt
    colour k dflt = case get k of
      Just s | s `elem` colourNames -> s
      _ -> dflt
    clamp lo hi = max lo . min hi
    clampR lo hi = max lo . min hi

-- | Only what differs from the defaults, so URLs stay short.
toQuery :: Params -> String
toQuery p = intercalate "&" [ urlEncode k ++ "=" ++ urlEncode v | (k, v) <- fields ]
  where
    d = defaultParams
    fields = concat
      [ [ ("g1", typeCode (pG1 p)) | pG1 p /= pG1 d ]
      , [ ("n", show (pN p)) | pN p /= pN d ]
      , [ ("g2", typeCode (pG2 p)) | pG2 p /= pG2 d ]
      , [ ("m", show (pM p)) | pM p /= pM d ]
      , [ ("scale", showRat (pScale p)) | pScale p /= pScale d ]
      , [ ("cx", showRat (pCx p)) | pCx p /= pCx d ]
      , [ ("edit", "1") | pEdit p ]
      , [ ("links", "1") | pLinks p ]
      , [ ("cusps", "0") | not (pCuspLabels p) ]
      , [ ("sel", show i) | Just i <- [pSel p] ]
      , [ ("mv", showMoves (pMoves p)) | not (null (pMoves p)) ]
      , [ ("fill", pFill p) | pFill p /= pFill d ]
      , [ ("outline", pOutline p) | pOutline p /= pOutline d ]
      , [ ("mode", "tri") | pMode p == TriMode ]
      , [ ("mats", showMats (pMats p)) | pMode p == TriMode, pMats p /= [identity] ]
      , [ ("copy", "1") | pCopy p ]
      , [ ("w", show (pW p)) | pW p /= pW d ]
      , [ ("h", show (pH p)) | pH p /= pH d ]
      , [ ("db", n) | Just n <- [pDb p] ]
      , [ ("view", "disk") | pView p == Disk ]
      , [ ("bg", "0") | not (pBg p) ]
      , [ ("tile", "1") | pTile p ]
      , [ ("c1", pC1 p) | pC1 p /= pC1 d ]
      , [ ("c2", pC2 p) | pC2 p /= pC2 d ]
      , [ ("fill2", pFill2 p) | pFill2 p /= pFill2 d ]
      , [ ("app", show (pApp p)) | pApp p /= 1 ]
      , [ ("gen", show g) | Just g <- [pGenus p] ]
      , [ ("lev", show l) | Just l <- [pLevel p] ]
      , [ ("idx", show i) | Just i <- [pIndex p] ]
      , [ ("by", b) | Just b <- [pBy p] ]
      , [ ("gens", pGens p) | pApp p == 3, not (null (pGens p)) ]
      ]

-- | Relative, so the same page works at the server's root and under a
-- project path such as @/FunDom/@ on GitHub Pages.
href :: Params -> String
href p = "?" ++ toQuery p
