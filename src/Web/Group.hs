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
module Web.Group (resolve, contextFrom, csgDirDefault) where

import Modular.CP
import Modular.Generators
import Modular.Group
import Web.Context
import Web.Params

csgDirDefault :: FilePath
csgDirDefault = "csg"

-- | The context, given the chosen table record (if any), the groups
-- matching the chosen filters, and the dropdown options (mode 2).
contextFrom :: Params -> Either String Record -> ([Summary], Options) -> Context
contextFrom p0 rec (matching, opts) = case pApp p0 of
  3 -> Context p0 (parseGenerators (pGens p0) >>= groupFromGenerators) opts []
  2 ->
    let -- a group named directly fixes the filters
        p = case (pGenus p0, pLevel p0, pIndex p0, rec) of
              (Nothing, Nothing, Nothing, Right r) -> p0 { pGenus = Just (rGenus r), pLevel = Just (rLevel r), pIndex = Just (rIndex r) }
              _ -> p0
        grp = case pDb p of
                Just _  -> toSubgroup <$> rec
                Nothing -> Left "choose a group"
    in Context p grp opts matching
  _ -> Context p0 (Right (subgroup (pG1 p0) (pN p0) (pG2 p0) (pM p0))) opts []

-- | With the tables on disk: @loader@ yields every summary (the server
-- memoises it, the command line scans).
resolve :: FilePath -> IO [Summary] -> Params -> IO Context
resolve dir loader p
  | pApp p /= 2 = return (contextFrom p (Left "no table lookup") ([], Options [] [] []))
  | otherwise = do
      rec <- case pDb p of
        Just name -> findRecord dir name
        Nothing   -> return (Left "choose a group")
      -- a name alone: filter by its own genus, level and index
      let filters = case (pGenus p, pLevel p, pIndex p, rec) of
            (Nothing, Nothing, Nothing, Right r) -> (Just (rGenus r), Just (rLevel r), Just (rIndex r))
            _ -> (pGenus p, pLevel p, pIndex p)
      sms <- loader
      return (contextFrom p rec (filterAndOptions filters sms))
