-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | Command line: the invariants of a group, its coset table, or the page
-- and picture for a query string — the same query string the server takes.
module Main (main) where

import Data.Array ((!))
import Flint.SL2
import Flint.Z
import Data.List (intercalate)
import Modular.CP
import Modular.Disk (sidePairings)
import Modular.Domain
import Modular.Generators
import Modular.Group
import Render.Page
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO
import Web.Context
import Web.Group
import Web.Params

usage :: String
usage = unlines
  [ "usage:"
  , "  fundom info  <type> <N> [<type> <M>]    index, genus, cusps, elliptic points"
  , "  fundom table <type> <N> [<type> <M>]    every coset representative and its side pairings"
  , "  fundom db    <name>                     a Cummins–Pauli group, e.g. 11A1, checked against the table"
  , "  fundom level <N>                        every group of level N in the tables"
  , "  fundom gens  '<a b c d; a b c d; …>'     the subgroup generated: index, level, congruence or not"
  , "  fundom csg-export                       the tables as JSON on stdout, for the browser build"
  , "  fundom svg   '<query>'                  the picture for a page query, e.g. 'g1=G0&n=11&scale=80'"
  , "  fundom page  '<query>'                  the whole HTML page"
  , "  fundom prequeries                       the page queries the browser build pre-renders, one per line"
  , "types: G0 = Γ₀, G1 = Γ₁, Gu0 = Γ⁰, Gu1 = Γ¹, G = Γ"
  , "the tables are read from $FUNDOM_CSG, default ./csg" ]

csgDir :: IO FilePath
csgDir = maybe csgDirDefault id <$> lookupEnv "FUNDOM_CSG"

main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs
  dir <- csgDir
  case args of
    ("info"  : rest) -> withGroup rest printInfo
    ("table" : rest) -> withGroup rest printTable
    ["db", name]     -> do
      r <- findRecord dir name
      case r of
        Left err  -> hPutStrLn stderr err >> exitFailure
        Right rec -> case enumerate (toSubgroup rec) of
          Left err  -> hPutStrLn stderr err >> exitFailure
          Right dom -> do
            printInfo dom
            putStrLn ("table:    index " ++ show (rIndex rec) ++ ", genus " ++ show (rGenus rec) ++ ", cusps " ++ unwords (map show (rCusps rec))
                      ++ ", e2 = " ++ show (length (filter (== 1) (rC2 rec))) ++ ", e3 = " ++ show (length (filter (== 1) (rC3 rec))))
    ["level", n]     -> do
      sms <- scanLevel dir (read n)
      mapM_ (\sm -> putStrLn (pad 8 (smName sm) ++ pad 22 (maybe "" prettyName (smSpecial sm)) ++ pad 8 ("μ=" ++ show (smIndex sm))
                                ++ pad 6 ("g=" ++ show (smGenus sm)) ++ unwords (map show (smCusps sm)))) sms
    ["gens", txt]    -> case parseGenerators txt >>= groupFromGenerators of
      Left err -> hPutStrLn stderr err >> exitFailure
      Right sg -> do
        case (sgVerdict sg, sgExpect sg) of
          (Just (lv, v), Just ex) -> do
            putStrLn ("index:    " ++ show (exIndex ex))
            putStrLn ("level:    " ++ show lv ++ " (generalised)")
            putStrLn ("cusps:    " ++ unwords (map show (exCusps ex)))
            putStrLn ("genus:    " ++ show (exGenus ex))
            putStrLn (maybe "congruence: yes" (\why -> "congruence: no (Hsu's relation " ++ why ++ " fails)") v)
          _ -> return ()
        either (hPutStrLn stderr) printInfo (enumerate sg)
    ["csg-export"]   -> allRecords dir >>= putStr . exportJson
    ["svg", q]       -> withParams dir q (putStr . renderSvgOnly)
    ["page", q]      -> withParams dir q (putStr . renderPage)
    -- the pages shown before the module has loaded: the default page (Γ), the empty tables and generators pages, the examples
    ["prequeries"]   -> mapM_ putStrLn ("" : "app=2" : "app=3" : [ toQuery defaultParams { pApp = 3, pGens = g } | (_, g) <- examples ])
    _                -> hPutStr stderr usage >> exitFailure
  where
    pad n s = s ++ replicate (max 1 (n - length s)) ' '

withParams :: FilePath -> String -> (Context -> IO ()) -> IO ()
withParams dir q k = resolve dir (scanAll dir) (parseParams (parseQuery q)) >>= k

withGroup :: [String] -> (Domain -> IO ()) -> IO ()
withGroup args k = case args of
  [t1, n]         | Just g1 <- typeFromCode t1                              -> go (subgroup g1 (read n) G0 1)
  [t1, n, t2, m]  | Just g1 <- typeFromCode t1, Just g2 <- typeFromCode t2 -> go (subgroup g1 (read n) g2 (read m))
  _ -> hPutStr stderr usage >> exitFailure
  where
    go sg = case enumerate sg of
      Left err  -> hPutStrLn stderr err >> exitFailure
      Right dom -> k dom

printInfo :: Domain -> IO ()
printInfo dom = do
  let i = info dom
      sg = dGroup dom
  putStrLn (subgroupName sg)
  putStrLn ("index:    " ++ show (iIndex i) ++ (if containsMinusOne sg then "" else "  (projective; -I is not in the group)"))
  putStrLn ("genus:    " ++ show (iGenus i))
  putStrLn ("cusps:    " ++ show (length (iCusps i)) ++ "   " ++ unwords [ showQI (cValue cu) ++ "·" ++ show (cWidth cu) | cu <- iCusps i ])
  putStrLn ("elliptic: e2 = " ++ show (iE2 i) ++ ", e3 = " ++ show (iE3 i))

printTable :: Domain -> IO ()
printTable dom = do
  printInfo dom
  putStrLn ("side pairings (generators): " ++ unwords [ let (a', b', c', d') = entries g in intercalate "," (map show [a', b', c', d']) | g <- sidePairings dom ])
  putStrLn ""
  putStrLn "#     matrix               cusp     T           T^-1        S"
  mapM_ line [0 .. size dom - 1]
  where
    line j = putStrLn $ pad 6 (show j) ++ pad 21 (showMat g) ++ pad 9 (showQI (cusp g)) ++ concatMap side gens
      where
        g = dReps dom ! j
        side gen = let e = edge dom j gen in pad 12 (show (eNbr e) ++ (if eGlued e then "" else "*"))
    pad n s = s ++ replicate (max 1 (n - length s)) ' '
