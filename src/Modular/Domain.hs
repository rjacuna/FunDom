{-# LANGUAGE DeriveDataTypeable #-}
-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | The fundamental domain as combinatorial data.
--
-- This is Helena A. Verrill's algorithm — "Algorithm for Drawing
-- Fundamental Domains", January 2001 — as implemented in her Fundamental
-- Domain Drawer applet (@RepList@ in the Java): the coset walk, the table
-- of side pairings with its glued/unglued flags, and the reading of cusps,
-- elliptic points and genus from it. The port changes the data structures
-- and the coset test, and the edit semantics ('applyMove'), not the idea.
--
-- A domain for Γ is a union of translates @g·F@ of the standard triangle
-- @F = {|z| ≥ 1, |Re z| ≤ ½}@, one per coset in Γ\\PSL₂(ℤ). Which
-- translates is a choice; this makes the one the applet made: a
-- breadth-first walk from the identity, multiplying on the right by
-- @T, T⁻¹, S@ in that order, so each new triangle is placed against the
-- triangle that discovered it and the domain grows outward from the cusp
-- at ∞ in a connected blob.
--
-- The walk also records, for every triangle and every side, which triangle
-- the side is identified with, and whether that triangle actually sits
-- across the side ("glued") or lies elsewhere in the picture ("linked",
-- i.e. the side is a boundary edge paired by a nontrivial element of Γ).
-- Cusps, elliptic points and the genus are read off that table.
module Modular.Domain
  ( Gen(..), gens, genMat, genInverse, genCode, genFromCode
  , Edge(..), Domain(..), enumerate, maxCosets
  , size, edge, neighbour
  , Cusp(..), Info(..), info
  , Move, applyMove, applyMoves, connected
  ) where

import Data.Array
import Data.Foldable (toList)
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Sq
import Flint.SL2
import Flint.Z (QI)
import Modular.Group

-- | The three sides of a triangle, named by the generator that carries @F@
-- across them: @T@ is the right side, @T⁻¹@ the left, @S@ the bottom arc.
data Gen = T | Tinv | S
  deriving (Eq, Ord, Show, Enum, Bounded, Ix)

gens :: [Gen]
gens = [T, Tinv, S]

genMat :: Gen -> SL2
genMat T    = genT
genMat Tinv = genTinv
genMat S    = genS

genInverse :: Gen -> Gen
genInverse T    = Tinv
genInverse Tinv = T
genInverse S    = S

genCode :: Gen -> Char
genCode T    = 'T'
genCode Tinv = 'U'
genCode S    = 'S'

genFromCode :: Char -> Maybe Gen
genFromCode 'T' = Just T
genFromCode 'U' = Just Tinv
genFromCode 'S' = Just S
genFromCode _   = Nothing

data Edge = Edge
  { eNbr   :: !Int    -- ^ the triangle this side is identified with
  , eGlued :: !Bool   -- ^ and whether it is drawn across this side
  } deriving (Eq, Show)

data Domain = Domain
  { dGroup :: Subgroup
  , dReps  :: Array Int SL2
  , dEdges :: Array (Int, Gen) Edge
  }

-- | Γ(N) has index ~N³/2; this keeps a typo from filling memory.
maxCosets :: Int
maxCosets = 20000

size :: Domain -> Int
size = rangeSize . bounds . dReps

edge :: Domain -> Int -> Gen -> Edge
edge dom i g = dEdges dom ! (i, g)

neighbour :: Domain -> Int -> Gen -> Int
neighbour dom i g = eNbr (edge dom i g)

enumerate :: Subgroup -> Either String Domain
enumerate sg = go 0 (Sq.singleton identity) (M.singleton (cosetKey sg identity) 0) M.empty
  where
    go i reps keys edges
      | Sq.length reps > maxCosets = Left ("more than " ++ show maxCosets ++ " cosets; lower the level")
      | i >= Sq.length reps        = Right (finish reps edges)
      | otherwise                  = go (i + 1) reps' keys' edges'
      where (reps', keys', edges') = foldl (visit i) (reps, keys, edges) gens

    visit i st@(reps, keys, edges) g
      | M.member (i, g) edges = st
      | otherwise =
          let m = Sq.index reps i `mul` genMat g
              k = cosetKey sg m
          in case M.lookup k keys of
               Nothing -> let j = Sq.length reps
                          in (reps Sq.|> m, M.insert k j keys, connect i g j True edges)
               Just j  -> (reps, keys, connect i g j (m == Sq.index reps j) edges)

    connect i g j glued =
      M.insert (j, genInverse g) (Edge i glued) . M.insert (i, g) (Edge j glued)

    finish reps edges =
      let n = Sq.length reps
      in Domain sg (listArray (0, n - 1) (toList reps))
                   (array ((0, T), (n - 1, S)) [ ((i, g), edges M.! (i, g)) | i <- [0 .. n - 1], g <- gens ])

-- Invariants ------------------------------------------------------------------

data Cusp = Cusp
  { cValue   :: QI     -- ^ g(∞) for the first triangle of the orbit
  , cWidth   :: !Int   -- ^ the size of its T-orbit
  , cMembers :: [Int]  -- ^ the triangles meeting at it
  } deriving (Show)

data Info = Info
  { iIndex :: !Int
  , iCusps :: [Cusp]
  , iE2    :: !Int     -- ^ elliptic points of order 2: sides identified with themselves by S
  , iE3    :: !Int     -- ^ of order 3: triangles fixed by T·S
  , iGenus :: !Int
  , iEuler :: !Bool    -- ^ whether 12(g−1) = μ − 3e₂ − 4e₃ − 6c came out integral
  } deriving (Show)

-- | Cusps are the orbits of T on the coset list, elliptic points the fixed
-- points of S and of T·S, and the genus is Euler's formula.
info :: Domain -> Info
info dom = Info n cusps e2 e3 g (r == 0)
  where
    n = size dom
    nbr i gen = neighbour dom i gen
    cusps = orbits (M.fromList [ (i, ()) | i <- [0 .. n - 1] ])
    orbits pending = case M.lookupMin pending of
      Nothing     -> []
      Just (i, _) ->
        let orbit = i : takeWhile (/= i) (drop 1 (iterate (`nbr` T) i))
        in Cusp (cusp (dReps dom ! i)) (length orbit) orbit
             : orbits (foldr M.delete pending orbit)
    e2 = length [ i | i <- [0 .. n - 1], nbr i S == i ]
    e3 = length [ i | i <- [0 .. n - 1], nbr (nbr i T) S == i ]
    (q, r) = (12 + n - 3 * e2 - 4 * e3 - 6 * length cusps) `divMod` 12
    g = q

-- Editing ---------------------------------------------------------------------

-- | Clicking the marker on an unglued side @(i, g)@ asks for the triangle
-- identified with that side to be drawn there instead.
type Move = (Int, Gen)

-- | Triangles reachable from @start@ across glued sides, never entering
-- @blocked@.
reach :: Domain -> IS.IntSet -> Int -> IS.IntSet
reach dom blocked start = go (IS.singleton start) (Sq.singleton start)
  where
    go seen queue = case Sq.viewl queue of
      Sq.EmptyL   -> seen
      k Sq.:< rest ->
        let nexts = [ w | h <- gens, let e = edge dom k h, eGlued e
                        , let w = eNbr e, not (IS.member w seen), not (IS.member w blocked) ]
        in go (foldr IS.insert seen nexts) (foldl (Sq.|>) rest nexts)

-- | Whether every triangle is reachable from the first across glued sides,
-- i.e. whether the picture is one region.
connected :: Domain -> Bool
connected dom = IS.size (reach dom IS.empty 0) == size dom

-- | Redraw the triangle @j@ paired with side @(i, g)@ across that side, at
-- @reps[i]·g@ — and bring with it everything that was attached to the
-- picture only through @j@. Those triangles move rigidly, by the same
-- left multiplication @γ = reps[i]·g·reps[j]⁻¹@, so their gluings to @j@
-- and to each other survive; the sides of the moved block are then
-- reglued against whatever they now sit next to.
--
-- The Java applet moved @j@ alone, which cut loose whatever hung off it:
-- one click on Γ₀(11) leaves the picture in two pieces touching at the
-- cusp 0. Here a move never disconnects a connected domain: the glued
-- graph is a tree, the block is the union of @j@'s subtrees away from
-- @i@'s side, and it comes back attached at @(i, g)@.
--
-- A side paired with itself (@j == i@: an elliptic point of order 2 on an
-- S-side, or the two sides of a width-one cusp) has nothing to bring, and
-- is left alone.
applyMove :: Move -> Domain -> Domain
applyMove (i, g) dom
  | i < 0 || i >= size dom = dom
  | eGlued e || j == i     = dom
  | otherwise = reglue dom { dReps = dReps dom // [ (k, gamma `mul` (dReps dom ! k)) | k <- IS.toList block ] }
  where
    e     = edge dom i g
    j     = eNbr e
    gamma = ((dReps dom ! i) `mul` genMat g) `mul` inv (dReps dom ! j)
    body  = reach dom (IS.singleton j) i      -- what stays: i's side of the tree, without j
    block = reach dom body j                  -- what moves: j and everything attached only through it

-- | Recompute every glued flag from the exact test: a side is glued iff the
-- matrix across it is the matrix of the triangle it is paired with.
reglue :: Domain -> Domain
reglue dom = dom { dEdges = listArray (bounds es) [ Edge w (((reps ! k) `mul` genMat h) == (reps ! w))
                                                  | ((k, h), Edge w _) <- assocs es ] }
  where
    es   = dEdges dom
    reps = dReps dom

applyMoves :: [Move] -> Domain -> Domain
applyMoves ms dom = foldl (flip applyMove) dom ms
