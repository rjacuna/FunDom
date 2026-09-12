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
  , Summary(..), scanLevel, scanGenus, scanAll, allNames, findRecordNamed, prettyName, displayName, specialTex
  , Options(..), Filters, filterAndOptions, compactOptions, parseOptions
  , compactRecord, parseCompact, compactSummary, parseSummaries, parseCandidates, parseNames, summaryOf, allRecords, exportJson
  ) where

import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit, isUpper)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
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
  , rCon      :: Int        -- ^ their @GLZConj@: conjugates under the outer automorphism
  , rLen      :: Int        -- ^ their @length@: the number of PSL₂(ℤ)-conjugates
  , rGal      :: [Int]      -- ^ their @gc@: orbit lengths under the Galois action
  , rSupers   :: [String]   -- ^ direct supergroups, by name
  , rSubs     :: [String]   -- ^ direct subgroups of genus ≤ 24, by name
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
        return (fst3 <$> parseRecord (B.unpack (B.concat [body, close, B.pack ">"])))
  where fst3 (a, _, _) = a

-- | The same, with super- and subgroups named: @names@ is the master list
-- ('allNames' or the names of 'scanAll', in order).
findRecordNamed :: FilePath -> [String] -> String -> IO (Either String Record)
findRecordNamed dir names name = case recordFile name of
  Nothing -> return (Left ("not a Cummins–Pauli name: " ++ name ++ " (expected e.g. 11A1)"))
  Just rel -> do
    let path = dir </> rel
    ok <- doesFileExist path
    if not ok then return (Left ("no table " ++ path ++ " — is the csg/ directory in place?")) else do
      bs <- normalise <$> B.readFile path
      let marker = B.pack ("name := \"" ++ name ++ "\"")
          (before, after) = B.breakSubstring marker bs
      if B.null after then return (Left ("no group " ++ name ++ " in " ++ rel)) else do
        let start = lastIndexOf (B.pack "rec<") before
            rest  = B.drop start bs
            (body, tl) = B.breakSubstring (B.pack "treesupergroups :=") rest
            close = B.takeWhile (/= '>') tl
        return (withNames names <$> parseRecord (B.unpack (B.concat [body, close, B.pack ">"])))

-- | Positions in the master list → names.
withNames :: [String] -> (Record, [Int], [Int]) -> Record
withNames names (r, sup, sub) = r { rSupers = map nm sup, rSubs = map nm sub }
  where
    arr = M.fromList (zip [1 :: Int ..] names)
    nm i = M.findWithDefault ("#" ++ show i) i arr

normalise :: B.ByteString -> B.ByteString
normalise = B.unwords . B.words

lastIndexOf :: B.ByteString -> B.ByteString -> Int
lastIndexOf pat s = go 0 0
  where
    go from best = case B.breakSubstring pat (B.drop from s) of
      (pre, post) | B.null post -> best
                  | otherwise -> let at = from + B.length pre in go (at + 1) at

-- | Parse one record's text.
-- | Parse one record's text. Super- and subgroups come out as positions in
-- the tables' master list; 'withNames' turns them into names.
parseRecord :: String -> Either String (Record, [Int], [Int])
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
  con   <- int "GLZConj"
  len   <- int "length"
  gal   <- ints "gc"
  sup   <- ints "direct_supergroups"
  sub   <- ints "direct_subgroups"
  return ( Record { rName = name, rLevel = lv, rIndex = ix, rGenus = gen, rMinusOne = mo
                  , rMatGens = mg, rCusps = cu, rC2 = c2, rC3 = c3, rSpecial = special
                  , rCon = con, rLen = len, rGal = gal, rSupers = [], rSubs = [] }
         , sup, sub )
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
    -- "[ IntegerRing() | 1, 11 ]" → [1, 11]; an empty list is written "[]"
    ints key = do
      r <- after key
      let block = takeWhile (/= ']') (drop 1 (dropWhile (/= '[') r))
          inner = if '|' `elem` block then drop 1 (dropWhile (/= '|') block) else ""
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
    name = displayName (rName r) (rSpecial r)

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

-- | How an entry is named in the interface: its classical name when it has
-- one, else its name in the tables.
displayName :: String -> Maybe String -> String
displayName nm = maybe nm prettyName

-- | A classical name as TeX for display: the bar (image in PSL₂) dropped.
specialTex :: String -> String
specialTex [] = []
specialTex s@(ch : rest)
  | take 9 s == "\\overline" = specialTex (drop 9 s)
  | otherwise = ch : specialTex rest

-- | One line of a listing.
data Summary = Summary
  { smName :: String, smLevel :: Int, smIndex :: Int, smGenus :: Int, smCusps :: [Int], smSpecial :: Maybe String
  , smCon :: Int, smLen :: Int, smGal :: [Int], smE2 :: Int, smE3 :: Int
  , smSupers :: [String], smSubs :: [String] }
  deriving (Show)

-- | The tables' name for a group, in their notation: level, label, genus.
summaryOfRecord :: Record -> Summary
summaryOfRecord r = Summary { smName = rName r, smLevel = rLevel r, smIndex = rIndex r, smGenus = rGenus r
                            , smCusps = rCusps r, smSpecial = rSpecial r, smCon = rCon r, smLen = rLen r, smGal = rGal r
                            , smE2 = length (filter (== 1) (rC2 r)), smE3 = length (filter (== 1) (rC3 r))
                            , smSupers = rSupers r, smSubs = rSubs r }

-- | Every group of a given level, across all genera. A cheap scan: only
-- the fields of the listing are read.
scanLevel :: FilePath -> Int -> IO [Summary]
scanLevel dir lv = filter ((== lv) . smLevel) <$> scanAll dir

-- | Every group of a genus, across all levels.
scanGenus :: FilePath -> Int -> IO [Summary]
scanGenus dir gen = filter ((== gen) . smGenus) <$> scanAll dir

-- | Every group in the tables, in the order of their master list, with
-- super- and subgroups named. 114 MB of text; the server does this once.
scanAll :: FilePath -> IO [Summary]
scanAll dir = do
  raw <- allRecordsRaw dir
  let names = [ rName r | (r, _, _) <- raw ]
  return [ summaryOfRecord (withNames names t) | t <- raw ]

-- | The names of every group, in master order.
allNames :: FilePath -> IO [String]
allNames dir = map (\(r, _, _) -> rName r) <$> allRecordsRaw dir

-- Browsing --------------------------------------------------------------------------

-- | The values each of genus, level and index may take, given the other
-- two filters — what the dropdowns offer.
data Options = Options { oGenera, oLevels, oIndices :: [Int] } deriving (Show, Eq)

-- | Chosen genus, level, index, any of them.
type Filters = (Maybe Int, Maybe Int, Maybe Int)

-- | The groups matching all chosen filters, and the options for each
-- category given the other two. The JavaScript of the browser build does
-- the same over its JSON.
filterAndOptions :: Filters -> [Summary] -> ([Summary], Options)
filterAndOptions (g, l, i) sms = (matching, Options (distinct smGenus (g', l, i)) (distinct smLevel (g, l', i)) (distinct smIndex (g, l, i')))
  where
    matching = [ s | s <- sms, fits (g, l, i) s ]
    fits (fg, fl, fi) s = maybe True (== smGenus s) fg && maybe True (== smLevel s) fl && maybe True (== smIndex s) fi
    (g', l', i') = (Nothing, Nothing, Nothing) :: Filters
    distinct f fs = sortNub [ f s | s <- sms, fits fs s ]
    sortNub = map head' . groupSorted . sortInts
    sortInts = foldr insertSorted []
    insertSorted x [] = [x]
    insertSorted x (y : ys) | x <= y = x : y : ys
                            | otherwise = y : insertSorted x ys
    groupSorted [] = []
    groupSorted (x : xs) = let (same, rest) = span (== x) xs in (x : same) : groupSorted rest
    head' (x : _) = x
    head' [] = 0

-- | @#gen 0 1 2|lev 1 2 3|idx 1 2 6@, the first line of the summaries handed
-- to the module.
compactOptions :: Options -> String
compactOptions o = "#gen " ++ unwords (map show (oGenera o)) ++ "|lev " ++ unwords (map show (oLevels o)) ++ "|idx " ++ unwords (map show (oIndices o))

parseOptions :: String -> Options
parseOptions txt = case [ l | l <- lines txt, take 1 l == "#" ] of
  (l : _) -> case splitOn '|' (drop 1 l) of
    [g, lv, ix] -> Options (nums g) (nums lv) (nums ix)
    _ -> Options [] [] []
  [] -> Options [] [] []
  where nums t = [ read w | w <- drop 1 (words t), all isDigit w, not (null w) ]

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
  , maybe "" id (rSpecial r)
  , show (rCon r), show (rLen r), unwords (map show (rGal r)), unwords (rSupers r), unwords (rSubs r) ]

parseCompact :: String -> Either String Record
parseCompact line = case splitOn '|' line of
  [name, lv, ix, gen, mo, mg, cu, c2, c3, sp, con, len, gal, sup, sub] ->
    Right Record { rName = name, rLevel = read lv, rIndex = read ix, rGenus = read gen, rMinusOne = mo == "1"
                 , rMatGens = [ (a, b, c, d) | m <- splitOn ';' mg, not (null m), [a, b, c, d] <- [map read (splitOn ',' m)] ]
                 , rCusps = map read (words cu), rC2 = map read (words c2), rC3 = map read (words c3)
                 , rSpecial = if null sp then Nothing else Just sp
                 , rCon = read con, rLen = read len, rGal = map read (words gal), rSupers = words sup, rSubs = words sub }
  _ -> Left ("malformed record: " ++ take 60 line)

-- | @name|level|index|genus|cusps|special|con|len|gal|e2|e3|supers|subs@.
compactSummary :: Summary -> String
compactSummary sm = intercalate "|"
  [ smName sm, show (smLevel sm), show (smIndex sm), show (smGenus sm), unwords (map show (smCusps sm)), maybe "" id (smSpecial sm)
  , show (smCon sm), show (smLen sm), unwords (map show (smGal sm)), show (smE2 sm), show (smE3 sm)
  , unwords (smSupers sm), unwords (smSubs sm) ]

parseSummaries :: String -> [Summary]
parseSummaries txt = [ sm | l <- lines txt, take 1 l `notElem` ["#", "!", "~"], Just sm <- [one l] ]
  where
    one l = case splitOn '|' l of
      [name, lv, ix, gen, cu, sp, con, len, gal, e2, e3, sup, sub]
        | all isDigit lv, all isDigit ix, all isDigit gen, not (null lv) ->
        Just Summary { smName = name, smLevel = read lv, smIndex = read ix, smGenus = read gen
                     , smCusps = map read (words cu), smSpecial = if null sp then Nothing else Just sp
                     , smCon = read con, smLen = read len, smGal = map read (words gal), smE2 = read e2, smE3 = read e3
                     , smSupers = words sup, smSubs = words sub }
      _ -> Nothing

-- | Candidate records for identification travel in the same string, one
-- per line, prefixed @!@.
parseCandidates :: String -> [Record]
parseCandidates txt = [ r | l <- lines txt, take 1 l == "!", Right r <- [parseCompact (drop 1 l)] ]

-- | The classical names of the entries a panel may link to travel as
-- @~name|special@ lines.
parseNames :: String -> [(String, String)]
parseNames txt = [ (nm, sp) | l <- lines txt, take 1 l == "~", [nm, sp] <- [splitOn '|' (drop 1 l)], not (null sp) ]

summaryOf :: Record -> Summary
summaryOf = summaryOfRecord

splitOn :: Char -> String -> [String]
splitOn ch s = case break (== ch) s of
  (w, [])    -> [w]
  (w, _ : r) -> w : splitOn ch r

-- | Every record in the tables, in master order, with links named.
allRecords :: FilePath -> IO [Record]
allRecords dir = do
  raw <- allRecordsRaw dir
  let names = [ rName r | (r, _, _) <- raw ]
  return (map (withNames names) raw)

-- | Every record, links as positions. The master list is the files in the
-- order they load one another: genus by genus, level bucket by bucket.
allRecordsRaw :: FilePath -> IO [(Record, [Int], [Int])]
allRecordsRaw dir = concat <$> mapM one [ (g, b) | g <- [0 .. 24 :: Int], b <- [0, 8 .. 512 :: Int] ]
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
            ++ ",\"s\":" ++ str (maybe "" id (rSpecial r))
            ++ ",\"con\":" ++ show (rCon r) ++ ",\"len\":" ++ show (rLen r) ++ ",\"gal\":" ++ ints (rGal r)
            ++ ",\"sup\":" ++ strs (rSupers r) ++ ",\"sub\":" ++ strs (rSubs r) ++ "}"
    ints xs = "[" ++ intercalate "," (map show xs) ++ "]"
    strs xs = "[" ++ intercalate "," (map str xs) ++ "]"
    str t = "\"" ++ concatMap (\c -> case c of { '"' -> "\\\""; '\\' -> "\\\\"; _ -> [c] }) t ++ "\""
