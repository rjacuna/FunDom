-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | The Cummins–Pauli tables of congruence subgroups of PSL₂(ℤ) of genus
-- ≤ 24, read straight from their Magma files in @csg/@.
--
-- A group is named @<level><label><genus>@, e.g. @11A1@, and lives in
-- @csg<genus>-lev<8·⌊level/8⌋>.dat@, so the name says which file to open.
-- Each file is a Magma sequence of records; the fields used here are
-- @level@, @index@, @genus@, @contains_minus_one@, @matgens@ (generators as
-- 2×2 matrices mod the level, row-major), @cusps@ (the cycle type of T on
-- the cosets, i.e. the cusp widths), @c2@ and @c3@ (the cycle types of S
-- and R, whose fixed points are the elliptic points), and @special_name@.
--
-- The tables list groups up to conjugacy in PGL₂(ℤ); the generators define
-- one representative, which need not be the classical group of the same
-- name (their @11A1@ is a conjugate of Γ₀(11)).
module Modular.CP
  ( Record(..), parseName, recordFile, parseRecord, findRecord, toSubgroup
  , Summary(..), scanLevel, scanGenus, prettyName
  , compactRecord, parseCompact, compactSummary, parseSummaries, summaryOf, allRecords, exportJson
  ) where

import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit, isUpper)
import Data.List (intercalate)
import Modular.Group
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data Record = Record
  { rName     :: String
  , rLevel    :: Int
  , rIndex    :: Int
  , rGenus    :: Int
  , rMinusOne :: Bool
  , rMatGens  :: [(Int, Int, Int, Int)]
  , rCusps    :: [Int]
  , rC2       :: [Int]
  , rC3       :: [Int]
  , rSpecial  :: Maybe String
  } deriving (Show)

-- | @11A1@ → (11, "A", 1).
parseName :: String -> Maybe (Int, String, Int)
parseName s =
  case span isDigit s of
    (lv@(_ : _), rest) -> case span isUpper rest of
      (lab@(_ : _), gen@(_ : _)) | all isDigit gen -> Just (read lv, lab, read gen)
      _ -> Nothing
    _ -> Nothing

-- | The file a group's record is in, relative to the tables directory.
recordFile :: String -> Maybe FilePath
recordFile s = do
  (lv, _, gen) <- parseName s
  return ("csg" ++ show gen ++ "-lev" ++ show (8 * (lv `div` 8)) ++ ".dat")

-- | Look a group up by name.
findRecord :: FilePath -> String -> IO (Either String Record)
findRecord dir name = case recordFile name of
  Nothing -> return (Left ("not a Cummins–Pauli name: " ++ name ++ " (expected e.g. 11A1)"))
  Just rel -> do
    let path = dir </> rel
    ok <- doesFileExist path
    if not ok then return (Left ("no table " ++ path ++ " — is the csg/ directory in place?")) else do
      -- Magma wraps its output at 80 columns, splitting "c3 := [" and
      -- friends across lines; fold all whitespace to single spaces first.
      bs <- normalise <$> B.readFile path
      let marker = B.pack ("name := \"" ++ name ++ "\"")
          (before, after) = B.breakSubstring marker bs
      if B.null after then return (Left ("no group " ++ name ++ " in " ++ rel)) else do
        -- the record runs from the last "rec<" before the name to the ">" that
        -- closes treesupergroups, the last field
        let start = lastIndexOf (B.pack "rec<") before
            rest  = B.drop start bs
            (body, tl) = B.breakSubstring (B.pack "treesupergroups :=") rest
            close = B.takeWhile (/= '>') tl
        return (parseRecord (B.unpack (B.concat [body, close, B.pack ">"])))

normalise :: B.ByteString -> B.ByteString
normalise = B.unwords . B.words

lastIndexOf :: B.ByteString -> B.ByteString -> Int
lastIndexOf pat s = go 0 0
  where
    go from best = case B.breakSubstring pat (B.drop from s) of
      (pre, post) | B.null post -> best
                  | otherwise -> let at = from + B.length pre in go (at + 1) at

-- | Parse one record's text.
parseRecord :: String -> Either String Record
parseRecord txt = do
  name  <- str "name"
  lv    <- int "level"
  ix    <- int "index"
  gen   <- int "genus"
  mo    <- bool "contains_minus_one"
  mg    <- matgens
  cu    <- ints "cusps"
  c2    <- ints "c2"
  c3    <- ints "c3"
  return Record { rName = name, rLevel = lv, rIndex = ix, rGenus = gen, rMinusOne = mo
                , rMatGens = mg, rCusps = cu, rC2 = c2, rC3 = c3, rSpecial = special }
  where
    after key = case breakOn (key ++ " := ") txt of
      Nothing -> Left ("field " ++ key ++ " missing")
      Just r  -> Right r
    int key = do
      r <- after key
      case span isDigit r of
        (ds@(_ : _), _) -> Right (read ds)
        _ -> Left ("field " ++ key ++ " is not an integer")
    bool key = do
      r <- after key
      Right (take 4 r == "true")
    str key = do
      r <- after key
      case r of
        '"' : t -> Right (takeWhile (/= '"') t)
        _ -> Left ("field " ++ key ++ " is not a string")
    -- "[ IntegerRing() | 1, 11 ]" → [1, 11]
    ints key = do
      r <- after key
      let inner = takeWhile (/= ']') (drop 1 (dropWhile (/= '|') r))
      Right (map read (words (map (\ch -> if ch == ',' then ' ' else ch) inner)))
    matgens = do
      r <- after "matgens"
      let (block, _) = spanBlock r      -- up to the matching close of the outer list
          rows = innerLists block
      Right [ (a, b, c, d) | [a, b, c, d] <- rows ]
    special = case breakOn "special_name := [" txt of
      Just r -> case dropWhile (/= '"') (takeWhile (/= ']') r) of
        '"' : t -> let s = takeWhile (/= '"') t in if null s then Nothing else Just s
        _ -> Nothing
      Nothing -> Nothing

-- | Text after the first occurrence of a substring.
breakOn :: String -> String -> Maybe String
breakOn pat = go
  where
    n = length pat
    go [] = Nothing
    go s@(_ : t) | take n s == pat = Just (drop n s)
                 | otherwise = go t

-- | The outer bracketed block: from the first "[" to its matching "]".
spanBlock :: String -> (String, String)
spanBlock s = case dropWhile (/= '[') s of
  [] -> ("", "")
  (_ : t) -> go (1 :: Int) t ""
  where
    go 0 rest acc = (reverse acc, rest)
    go _ [] acc = (reverse acc, "")
    go depth (ch : rest) acc
      | ch == '[' = go (depth + 1) rest (ch : acc)
      | ch == ']' = if depth == 1 then (reverse acc, rest) else go (depth - 1) rest (ch : acc)
      | otherwise = go depth rest (ch : acc)

-- | Every "[ ... | a, b, c, d ]" inside a block, as integer lists.
innerLists :: String -> [[Int]]
innerLists s = case dropWhile (/= '[') s of
  [] -> []
  (_ : t) -> let (body, rest) = break (== ']') t
                 nums = drop 1 (dropWhile (/= '|') body)
             in map read (words (map (\ch -> if ch == ',' then ' ' else ch) nums)) : innerLists (drop 1 rest)

-- | The group the record defines, with its invariants attached for checking.
toSubgroup :: Record -> Subgroup
toSubgroup r = (fromMatrices name (rLevel r) (rMinusOne r) (rMatGens r))
  { sgExpect = Just Expect
      { exSource = "Cummins–Pauli", exIndex = rIndex r, exGenus = rGenus r, exCusps = rCusps r
      , exE2 = length (filter (== 1) (rC2 r)), exE3 = length (filter (== 1) (rC3 r))
      , exSpecial = rSpecial r } }
  where
    name = rName r ++ maybe "" (\s -> " = " ++ prettyName s) (rSpecial r)

-- | The tables' classical names are LaTeX: @\overline\Gamma_0(11)@ is
-- their Γ₀(11) (the bar for the image in PSL₂). Enough of it in Unicode
-- for a label.
prettyName :: String -> String
prettyName = go
  where
    go [] = []
    go s@(ch : rest) = case [ (t, r) | (pat, t) <- table, Just r <- [stripPrefix' pat s] ] of
      ((t, r) : _) -> t ++ go r
      []           -> ch : go rest
    table = [ ("\\overline", ""), ("\\Gamma", "Γ"), ("\\cap", "∩"), ("\\pm", "±")
            , ("_0", "₀"), ("_1", "₁"), ("^0", "⁰"), ("^1", "¹"), ("\\,", " "), ("\\ ", " ") ]
    stripPrefix' pat str = if take (length pat) str == pat then Just (drop (length pat) str) else Nothing

-- | One line of a listing.
data Summary = Summary
  { smName :: String, smLevel :: Int, smIndex :: Int, smGenus :: Int, smCusps :: [Int], smSpecial :: Maybe String }
  deriving (Show)

-- | Every group of a given level, across all genera. A cheap scan: only
-- the fields of the listing are read.
scanLevel :: FilePath -> Int -> IO [Summary]
scanLevel dir lv = filter ((== lv) . smLevel) . concat <$> mapM (\gen -> scanFile dir gen (8 * (lv `div` 8))) [0 .. 24]

-- | Every group of a genus, across all levels: the files @csg<g>-lev*.dat@.
scanGenus :: FilePath -> Int -> IO [Summary]
scanGenus dir gen = concat <$> mapM (scanFile dir gen) [0, 8 .. 512]

scanFile :: FilePath -> Int -> Int -> IO [Summary]
scanFile dir gen bucket = do
      let path = dir </> ("csg" ++ show gen ++ "-lev" ++ show bucket ++ ".dat")
      ok <- doesFileExist path
      if not ok then return [] else do
        bs <- normalise <$> B.readFile path
        return [ s | chunk <- drop 1 (splitOn (B.pack "rec<") bs), Just s <- [summary chunk] ]
  where
    summary chunk = do
        lvl  <- field "level := " chunk
        name <- fmap (takeWhile (/= '"') . drop 1) (afterB "name := " chunk)
        ix   <- field "index := " chunk
        gen  <- field "genus := " chunk
        cu   <- fmap (\r -> map read (words (map (\ch -> if ch == ',' then ' ' else ch) (takeWhile (/= ']') (drop 1 (dropWhile (/= '|') r)))))) (afterB "cusps := " chunk)
        let sp = case afterB "special_name := [" chunk of
                   Just r -> case dropWhile (/= '"') (takeWhile (/= ']') r) of
                     '"' : t -> let s = takeWhile (/= '"') t in if null s then Nothing else Just s
                     _ -> Nothing
                   Nothing -> Nothing
        return Summary { smName = name, smLevel = read lvl, smIndex = read ix, smGenus = read gen, smCusps = cu, smSpecial = sp }
    afterB key chunk = let (_, post) = B.breakSubstring (B.pack key) chunk
                       in if B.null post then Nothing else Just (B.unpack (B.take 4000 (B.drop (length key) post)))
    field key chunk = fmap (takeWhile isDigit) (afterB key chunk)
    splitOn pat bs = case B.breakSubstring pat bs of
      (pre, post) | B.null post -> [pre]
                  | otherwise -> pre : splitOn pat (B.drop (B.length pat) post)

-- Compact wire formats -----------------------------------------------------------------
--
-- The browser build cannot read 114 MB of Magma; it loads a JSON export of
-- the fields used here (@exportJson@) and hands the wasm module one record
-- and one list of summaries per request, in the line formats below.

-- | @name|level|index|genus|minusOne|a,b,c,d;a,b,c,d;…|cusps|c2|c3|special@,
-- lists space-separated.
compactRecord :: Record -> String
compactRecord r = intercalate "|"
  [ rName r, show (rLevel r), show (rIndex r), show (rGenus r), if rMinusOne r then "1" else "0"
  , intercalate ";" [ intercalate "," (map show [a, b, c, d]) | (a, b, c, d) <- rMatGens r ]
  , unwords (map show (rCusps r)), unwords (map show (rC2 r)), unwords (map show (rC3 r))
  , maybe "" id (rSpecial r) ]

parseCompact :: String -> Either String Record
parseCompact line = case splitOn '|' line of
  [name, lv, ix, gen, mo, mg, cu, c2, c3, sp] ->
    Right Record { rName = name, rLevel = read lv, rIndex = read ix, rGenus = read gen, rMinusOne = mo == "1"
                 , rMatGens = [ (a, b, c, d) | m <- splitOn ';' mg, not (null m), [a, b, c, d] <- [map read (splitOn ',' m)] ]
                 , rCusps = map read (words cu), rC2 = map read (words c2), rC3 = map read (words c3)
                 , rSpecial = if null sp then Nothing else Just sp }
  _ -> Left ("malformed record: " ++ take 60 line)

-- | @name|level|index|genus|cusps|special@.
compactSummary :: Summary -> String
compactSummary sm = intercalate "|" [ smName sm, show (smLevel sm), show (smIndex sm), show (smGenus sm)
                                    , unwords (map show (smCusps sm)), maybe "" id (smSpecial sm) ]

parseSummaries :: String -> [Summary]
parseSummaries txt = [ sm | l <- lines txt, Just sm <- [one l] ]
  where
    one l = case splitOn '|' l of
      [name, lv, ix, gen, cu, sp] | all isDigit lv, all isDigit ix, all isDigit gen, not (null lv) ->
        Just Summary { smName = name, smLevel = read lv, smIndex = read ix, smGenus = read gen
                     , smCusps = map read (words cu), smSpecial = if null sp then Nothing else Just sp }
      _ -> Nothing

summaryOf :: Record -> Summary
summaryOf r = Summary { smName = rName r, smLevel = rLevel r, smIndex = rIndex r, smGenus = rGenus r
                      , smCusps = rCusps r, smSpecial = rSpecial r }

splitOn :: Char -> String -> [String]
splitOn ch s = case break (== ch) s of
  (w, [])    -> [w]
  (w, _ : r) -> w : splitOn ch r

-- | Every record in the tables.
allRecords :: FilePath -> IO [Record]
allRecords dir = concat <$> mapM one [ (g, b) | g <- [0 .. 24 :: Int], b <- [0, 8 .. 512 :: Int] ]
  where
    one (gen, bucket) = do
      let path = dir </> ("csg" ++ show gen ++ "-lev" ++ show bucket ++ ".dat")
      ok <- doesFileExist path
      if not ok then return [] else do
        bs <- normalise <$> B.readFile path
        return [ r | chunk <- drop 1 (splitChunks (B.pack "rec<") bs), Right r <- [parseRecord (B.unpack chunk)] ]
    splitChunks pat bs = case B.breakSubstring pat bs of
      (pre, post) | B.null post -> [pre]
                  | otherwise -> pre : splitChunks pat (B.drop (B.length pat) post)

-- | The tables as JSON, one object per group, for the browser build.
exportJson :: [Record] -> String
exportJson rs = "[" ++ intercalate ",\n" (map obj rs) ++ "]\n"
  where
    obj r = "{\"n\":" ++ str (rName r) ++ ",\"l\":" ++ show (rLevel r) ++ ",\"i\":" ++ show (rIndex r)
            ++ ",\"g\":" ++ show (rGenus r) ++ ",\"m\":" ++ (if rMinusOne r then "1" else "0")
            ++ ",\"mg\":[" ++ intercalate "," [ "[" ++ intercalate "," (map show [a, b, c, d]) ++ "]" | (a, b, c, d) <- rMatGens r ] ++ "]"
            ++ ",\"cu\":" ++ ints (rCusps r) ++ ",\"c2\":" ++ ints (rC2 r) ++ ",\"c3\":" ++ ints (rC3 r)
            ++ ",\"s\":" ++ str (maybe "" id (rSpecial r)) ++ "}"
    ints xs = "[" ++ intercalate "," (map show xs) ++ "]"
    str t = "\"" ++ concatMap (\c -> case c of { '"' -> "\\\""; '\\' -> "\\\\"; _ -> [c] }) t ++ "\""
