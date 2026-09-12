-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | From the URL to a 'Context'.
--
-- 'contextFrom' is pure: it takes the record and the summaries the mode
-- needs, however they were obtained. 'resolve' obtains them from the
-- tables on disk, for the server and the command line; the browser build
-- obtains them from a JSON export and calls 'contextFrom' directly.
module Web.Group (resolve, contextFrom, csgDirDefault) where

import Data.List (nub, sort)
import Modular.CP
import Modular.Generators
import Modular.Group
import Web.Context
import Web.Params

csgDirDefault :: FilePath
csgDirDefault = "csg"

-- | The context, given the chosen table record (if any) and the summaries
-- of every group of the chosen genus (mode 2).
contextFrom :: Params -> Either String Record -> [Summary] -> Context
contextFrom p0 rec sms = case pApp p0 of
  3 -> Context p0 (parseGenerators (pGens p0) >>= groupFromGenerators) [] [] []
  2 ->
    let -- a group named directly fixes the browsing position
        p = case (pGenus p0, rec) of
              (Nothing, Right r) -> p0 { pGenus = Just (rGenus r), pLevel = Just (rLevel r), pIndex = Just (rIndex r) }
              _ -> p0
        levels  = sort (nub (map smLevel sms))
        atLevel = [ s | s <- sms, Just (smLevel s) == pLevel p ]
        indices = sort (nub (map smIndex atLevel))
        cls     = [ s | s <- atLevel, Just (smIndex s) == pIndex p ]
        grp     = case pDb p of
                    Just _  -> toSubgroup <$> rec
                    Nothing -> Left "choose a group"
    in Context p grp levels indices cls
  _ -> Context p0 (Right (subgroup (pG1 p0) (pN p0) (pG2 p0) (pM p0))) [] [] []

resolve :: FilePath -> Params -> IO Context
resolve dir p
  | pApp p /= 2 = return (contextFrom p (Left "no table lookup") [])
  | otherwise = do
      rec <- case pDb p of
        Just name -> findRecord dir name
        Nothing   -> return (Left "choose a group")
      let genus = case (pGenus p, rec) of
            (Just g, _)        -> Just g
            (Nothing, Right r) -> Just (rGenus r)
            _                  -> Nothing
      sms <- maybe (return []) (scanGenus dir) genus
      return (contextFrom p rec sms)
