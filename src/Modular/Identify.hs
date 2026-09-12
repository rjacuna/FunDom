-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. SPDX-License-Identifier: GPL-3.0-or-later
-- | Which entry of the tables a group is.
--
-- The tables list congruence subgroups up to conjugacy in PGL₂(ℤ). Two
-- subgroups are conjugate in PSL₂(ℤ) iff their actions on their cosets are
-- isomorphic as PSL₂(ℤ)-sets — a bijection of cosets commuting with S and
-- T — and conjugate in PGL₂(ℤ) iff that holds for the action or for its
-- mirror, the action conjugated by @diag(1, −1)@, which fixes S and inverts
-- T. Both actions are read straight off a domain's gluing table, and the
-- bijection, if it exists, is determined by where it sends one coset.
module Modular.Identify
  ( Key, keyOf, showKey, candidatesOf, sameGroup, identify
  ) where

import Data.Array
import Data.List (sort)
import Modular.CP
import Modular.Domain

-- | Genus, generalised level (lcm of the cusp widths), index, and the cusp
-- widths sorted: what the tables can be searched by.
type Key = (Int, Int, Int, [Int])

keyOf :: Domain -> Key
keyOf dom = (iGenus inf, foldr lcm 1 widths, size dom, sort widths)
  where
    inf = info dom
    widths = map cWidth (iCusps inf)

-- | @genus|level|index|w w w@, for the browser's filter.
showKey :: Key -> String
showKey (g, l, i, ws) = show g ++ "|" ++ show l ++ "|" ++ show i ++ "|" ++ unwords (map show ws)

candidatesOf :: Key -> [Summary] -> [Summary]
candidatesOf (g, l, i, ws) = filter (\s -> smGenus s == g && smLevel s == l && smIndex s == i && sort (smCusps s) == ws)

-- | The coset action from the gluing table: the images of each coset under S and T.
actionOf :: Domain -> (Array Int Int, Array Int Int)
actionOf dom = (tab S, tab T)
  where
    n = size dom
    tab g = listArray (0, n - 1) [ neighbour dom c g | c <- [0 .. n - 1] ]

-- | Conjugate in PGL₂(ℤ).
sameGroup :: Domain -> Domain -> Bool
sameGroup a b
  | size a /= size b = False
  | otherwise = any (iso sA tA sB tB) [0 .. n - 1] || any (iso sA tA sB tBinv) [0 .. n - 1]
  where
    n = size a
    (sA, tA) = actionOf a
    (sB, tB) = actionOf b
    tBinv = array (0, n - 1) [ (tB ! c, c) | c <- [0 .. n - 1] ]
    -- the map sending coset 0 of a to coset c of b, propagated along S and T
    iso s1 t1 s2 t2 c0 = go (listArray (0, n - 1) (repeat (-1)) // [(0, c0)]) [0]
      where
        go :: Array Int Int -> [Int] -> Bool
        go _ [] = True
        go f (x : queue) =
          let y = f ! x
              steps = [ (s1 ! x, s2 ! y), (t1 ! x, t2 ! y) ]
              check f' [] = Just (f', [])
              check f' ((x', y') : more) = case f' ! x' of
                v | v == -1 -> fmap (\(f'', q) -> (f'' // [(x', y')], x' : q)) (check f' more)
                  | v == y' -> check f' more
                  | otherwise -> Nothing
          in case check f steps of
               Nothing -> False
               Just (f', new) -> go f' (queue ++ new)

-- | The record whose group is the given one, among candidates.
identify :: Domain -> [Record] -> Maybe Record
identify dom = go
  where
    go [] = Nothing
    go (r : rs) = case enumerate (toSubgroup r) of
      Right d | sameGroup dom d -> Just r
      _ -> go rs
