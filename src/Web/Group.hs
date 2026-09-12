-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | From the URL to a 'Context'.
--
-- 'contextFrom' is pure: it takes the record and the browsing data the mode
-- needs, however they were obtained. 'resolve' obtains them from the tables
-- on disk, for the server and the command line; the browser build obtains
-- them from a JSON export and calls 'contextFrom' directly.
module Web.Group (resolve, contextFrom, groupOnly, csgDirDefault) where

import Modular.CP
import Modular.Domain (enumerate)
import Modular.Generators
import Modular.Group
import Modular.Identify
import Web.Context
import Web.Params

csgDirDefault :: FilePath
csgDirDefault = "csg"

-- | The context, given the chosen table record (if any), the groups
-- matching the chosen filters with the dropdown options (mode 2), and the
-- table entries that might be this group (modes 1 and 3).
contextFrom :: Params -> Either String Record -> ([Summary], Options) -> [Record] -> Context
contextFrom p0 rec (matching, opts) cands = case pApp p0 of
  3 -> Context p0 (parseGenerators (pGens p0) >>= groupFromGenerators) Nothing opts [] cands
  2 ->
    let -- a group named directly fixes the filters
        p = case (pGenus p0, pLevel p0, pIndex p0, rec) of
              (Nothing, Nothing, Nothing, Right r) -> p0 { pGenus = Just (rGenus r), pLevel = Just (rLevel r), pIndex = Just (rIndex r) }
              _ -> p0
        grp = case pDb p of
                Just _  -> toSubgroup <$> rec
                Nothing -> Left "choose a group"
    in Context p grp (either (const Nothing) Just rec) opts matching []
  _ -> Context p0 (Right (subgroup (pG1 p0) (pN p0) (pG2 p0) (pM p0))) Nothing opts [] cands

-- | The group of modes 1 and 3 alone, for finding its key.
groupOnly :: Params -> Either String Subgroup
groupOnly p = case pApp p of
  3 -> parseGenerators (pGens p) >>= groupFromGenerators
  _ -> Right (subgroup (pG1 p) (pN p) (pG2 p) (pM p))

-- | With the tables on disk: @loader@ yields every summary (the server
-- memoises it, the command line scans).
resolve :: FilePath -> IO [Summary] -> Params -> IO Context
resolve dir loader p = case pApp p of
  2 -> do
    sms <- loader
    let names = map smName sms
    rec <- case pDb p of
      Just name -> findRecordNamed dir names name
      Nothing   -> return (Left "choose a group")
    -- a name alone: filter by its own genus, level and index
    let filters = case (pGenus p, pLevel p, pIndex p, rec) of
          (Nothing, Nothing, Nothing, Right r) -> (Just (rGenus r), Just (rLevel r), Just (rIndex r))
          _ -> (pGenus p, pLevel p, pIndex p)
    return (contextFrom p rec (filterAndOptions filters sms) [])
  _ -> do
    -- the table entries this group might be, by genus, level, index and widths
    cands <- case groupOnly p >>= enumerate of
      Right dom -> do
        sms <- loader
        let names = map smName sms
        rs <- mapM (findRecordNamed dir names . smName) (candidatesOf (keyOf dom) sms)
        return [ r | Right r <- rs ]
      Left _ -> return []
    return (contextFrom p (Left "no table lookup") ([], Options [] [] []) cands)
