-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | Mode 3: from a list of integer matrices to a group with a verdict.
module Modular.Generators
  ( parseGenerators, showGenerators, groupFromGenerators, examples
  ) where

import Data.Array.Unboxed ((!))
import Data.Char (isDigit)
import Data.List (intercalate)
import Flint.SL2
import Modular.Congruence
import Modular.Coset
import Modular.Group
import Modular.Word

-- | Integers in any layout, four per matrix, row-major: @a b c d@.
parseGenerators :: String -> Either String [SL2]
parseGenerators txt
  | null nums = Left "enter generators: four integers a b c d per matrix, one matrix per line"
  | length nums `mod` 4 /= 0 = Left ("read " ++ show (length nums) ++ " integers, not a multiple of four")
  | otherwise = mapM one (zip [1 :: Int ..] (chunks nums))
  where
    nums = tokens txt
    tokens [] = []
    tokens (c : rest)
      | c == '-' || isDigit c =
          let (ds, r) = span isDigit rest
          in if c == '-' && null ds then tokens rest else read (c : ds) : tokens r
      | otherwise = tokens rest
    chunks [] = []
    chunks xs = take 4 xs : chunks (drop 4 xs)
    one (k, [a, b, c, d]) = case sl2 a b c d of
      Just g  -> Right g
      Nothing -> Left ("matrix " ++ show k ++ " = [" ++ show a ++ " " ++ show b ++ "; " ++ show c ++ " " ++ show d
                       ++ "] has determinant " ++ show (a * d - b * c) ++ ", not 1")
    one _ = Left "internal: chunk"

showGenerators :: [SL2] -> String
showGenerators gs = intercalate "\n" [ unwords (map show [a, b, c, d]) | g <- gs, let (a, b, c, d) = entries g ]

-- | Enumerate the cosets, decide congruence, and package the group so the
-- domain can be drawn from the coset action. The invariants of the action
-- are attached as what the domain must reproduce.
groupFromGenerators :: [SL2] -> Either String Subgroup
groupFromGenerators gs = case enumerateMatrices gs of
  Infinite k -> Left ("the subgroup generated has infinite index: the coset table does not close (" ++ show k
                      ++ " cosets before it stops growing), so it is not a congruence subgroup")
  Finite pr ->
    let (lv, verdict) = certify pr
        n  = prSize pr
        e2 = length [ c | c <- [0 .. n - 1], prS pr ! c == c ]
        e3 = length [ c | c <- [0 .. n - 1], prR pr ! c == c ]
        widths = cycleLengths (permL pr)
        g  = 1 + (n - 3 * e2 - 4 * e3 - 6 * length widths) `div` 12
        name = "⟨" ++ intercalate ", " (map showMat gs) ++ "⟩"
    in Right (fromAction name lv True (\m -> actWord pr (wordOf m) 0))
         { sgExpect  = Just Expect { exSource = "the coset action", exIndex = n, exGenus = g, exCusps = widths
                                   , exE2 = e2, exE3 = e3, exSpecial = Nothing }
         , sgVerdict = Just (lv, case verdict of { Congruence -> Nothing; NotCongruence why -> Just why }) }

-- | Worked examples for the generator box.
examples :: [(String, String)]
examples =
  [ ("Γ₀(2) = ⟨T, [1 0; 2 1]⟩", "1 1 0 1\n1 0 2 1")
  , ("Γ(2) = ⟨T², [1 0; 2 1]⟩", "1 2 0 1\n1 0 2 1")
  , ("Γ₀(11), from the side pairings of its domain", "1 1 0 1\n-5 -1 11 2\n-4 1 11 -3\n-3 1 11 -4\n-2 -1 11 5\n3 1 11 4\n4 1 11 3")
  , ("index 7, not congruence", "2 -1 1 0\n0 -1 1 -2\n-1 -2 1 1\n-1 -1 3 2")
  ]
