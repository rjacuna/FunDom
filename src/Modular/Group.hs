-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | Congruence subgroups, and the one thing the enumeration needs from them:
-- a canonical name for the coset @Γ·g@.
--
-- Right cosets are identified by @A ~ B  ⟺  A·B⁻¹ ∈ Γ@. For the five
-- classical families that condition reads off a residue of @A@ mod @N@:
--
-- > Γ₀(N)  c ≡ 0            the point (c : d) of P¹(ℤ/N)
-- > Γ₁(N)  c ≡ 0, a ≡ d ≡ 1 the bottom row (c, d) mod N, up to sign
-- > Γ⁰(N)  b ≡ 0            the point (a : b) of P¹(ℤ/N)
-- > Γ¹(N)  b ≡ 0, a ≡ d ≡ 1 the top row (a, b) mod N, up to sign
-- > Γ(N)   ≡ ±I             the whole matrix mod N, up to sign
--
-- For a group given by generators (the Cummins–Pauli tables), Γ is the
-- preimage of @H = ⟨generators, −I⟩ ≤ SL₂(ℤ/N)@, and the coset @Γ·g@ is the
-- coset @H·(g mod N)@, named by its smallest element.
--
-- "Up to sign" throughout, because the picture is of ±Γ\\ℍ: −I acts
-- trivially, and the index reported is the projective one.
module Modular.Group
  ( GroupType(..), allTypes, typeCode, typeFromCode, typeLabel
  , Subgroup(..), Expect(..), subgroup, fromMatrices, fromAction
  , subgroupName, containsMinusOne, cosetKey
  , Key, unitsMod, sl2Order
  ) where

import Flint.FFI (n_gcd)
import qualified Data.Set as Set
import Flint.SL2
import System.IO.Unsafe (unsafePerformIO)

data GroupType = G0 | G1 | Gup0 | Gup1 | Gfull
  deriving (Eq, Ord, Show, Enum, Bounded)

allTypes :: [GroupType]
allTypes = [minBound .. maxBound]

-- | Short codes for URLs.
typeCode :: GroupType -> String
typeCode G0    = "G0"
typeCode G1    = "G1"
typeCode Gup0  = "Gu0"
typeCode Gup1  = "Gu1"
typeCode Gfull = "G"

typeFromCode :: String -> Maybe GroupType
typeFromCode s = lookup s [ (typeCode t, t) | t <- allTypes ]

-- | The name with the level left as a placeholder.
typeLabel :: GroupType -> String -> String
typeLabel G0    n = "Γ₀(" ++ n ++ ")"
typeLabel G1    n = "Γ₁(" ++ n ++ ")"
typeLabel Gup0  n = "Γ⁰(" ++ n ++ ")"
typeLabel Gup1  n = "Γ¹(" ++ n ++ ")"
typeLabel Gfull n = "Γ("  ++ n ++ ")"

newtype Key = Key [Int] deriving (Eq, Ord, Show)

-- | A subgroup, as far as the drawer is concerned.
data Subgroup = Subgroup
  { sgName     :: String
  , sgLevel    :: Int
  , sgKey      :: SL2 -> Key     -- ^ canonical name of the coset Γ·g
  , sgMinusOne :: Bool           -- ^ −I ∈ Γ, so the projective index is the index
  , sgOrderH   :: Maybe Int      -- ^ |⟨generators, −I⟩| in SL₂(ℤ/N), for groups given that way
  , sgExpect   :: Maybe Expect   -- ^ what the source says, to check against
  , sgVerdict  :: Maybe (Int, Maybe String)  -- ^ generalised level, and the failing Hsu relation if not congruence
  }

-- | Invariants recorded by the source of a group (the Cummins–Pauli tables).
data Expect = Expect
  { exSource  :: String          -- ^ who says so
  , exIndex   :: Int
  , exGenus   :: Int
  , exCusps   :: [Int]           -- ^ cusp widths
  , exE2      :: Int
  , exE3      :: Int
  , exSpecial :: Maybe String    -- ^ a classical name, when they give one
  } deriving (Show)

subgroupName :: Subgroup -> String
subgroupName = sgName

containsMinusOne :: Subgroup -> Bool
containsMinusOne = sgMinusOne

cosetKey :: Subgroup -> SL2 -> Key
cosetKey = sgKey

-- The classical families -------------------------------------------------------

-- | The units of ℤ/N, by FLINT's @n_gcd@: what makes a projective point canonical.
unitsMod :: Int -> [Int]
unitsMod n = [ u | u <- [1 .. n - 1], gcdN u == 1 ]
  where gcdN u = unsafePerformIO (n_gcd (fromIntegral u) (fromIntegral n))

-- | An intersection of two classical groups. Level 1 makes a factor the whole group.
subgroup :: GroupType -> Int -> GroupType -> Int -> Subgroup
subgroup t1 n t2 m = Subgroup
  { sgName     = name
  , sgLevel    = lcm n m
  , sgKey      = \g -> Key (keyG t1 n us1 g ++ keyG t2 m us2 g)
  , sgMinusOne = has t1 n && has t2 m
  , sgOrderH   = Nothing
  , sgExpect   = Nothing
  , sgVerdict  = Nothing
  }
  where
    us1 = unitsMod n
    us2 = unitsMod m
    name | n == 1 && m == 1 = "SL₂(ℤ)"
         | m == 1 = typeLabel t1 (show n)
         | n == 1 = typeLabel t2 (show m)
         | otherwise = typeLabel t1 (show n) ++ " ∩ " ++ typeLabel t2 (show m)
    has t l = case t of
      G0   -> True
      Gup0 -> True
      _    -> l <= 2

-- | The residue that names the coset, for one classical group. Keys of the
-- two factors are concatenated; each has a fixed length, so that is injective.
keyG :: GroupType -> Int -> [Int] -> SL2 -> [Int]
keyG t n us m
  | n == 1    = []
  | otherwise = case t of
      G0    -> proj     [c', d']
      G1    -> upToSign [c', d']
      Gup0  -> proj     [a', b']
      Gup1  -> upToSign [a', b']
      Gfull -> upToSign [a', b', c', d']
  where
    (a0, b0, c0, d0) = entries m
    r x = fromInteger (x `mod` toInteger n) :: Int
    a' = r a0
    b' = r b0
    c' = r c0
    d' = r d0
    proj v     = minimum [ map (\x -> x * u `mod` n) v | u <- us ]
    upToSign v = min v (map (\x -> (n - x) `mod` n) v)

-- Groups given by generators mod N -------------------------------------------------

type Mat4 = (Int, Int, Int, Int)

mulMod :: Int -> Mat4 -> Mat4 -> Mat4
mulMod n (a, b, c, d) (e, f, g, h) =
  ((a * e + b * g) `mod` n, (a * f + b * h) `mod` n, (c * e + d * g) `mod` n, (c * f + d * h) `mod` n)

-- | The subgroup of SL₂(ℤ/N) generated: closure under right multiplication
-- by the generators, which for a finite group is the whole subgroup.
closure :: Int -> [Mat4] -> [Mat4]
closure n gens = go (Set.singleton one) [one]
  where
    one = (1, 0, 0, 1)
    go seen [] = Set.toList seen
    go seen (x : queue) =
      let new = [ y | s <- gens, let y = mulMod n x s, not (Set.member y seen) ]
      in go (foldr Set.insert seen new) (queue ++ new)

-- | A congruence subgroup of level @N@ from generators mod @N@: the preimage
-- of @H = ⟨generators, −I⟩@. The coset of @g@ is @H·(g mod N)@, named by its
-- smallest element — a minimum over @H@, which is fast enough for the
-- tables (|H| is at most a few tens of thousands) if not elegant.
fromMatrices :: String -> Int -> Bool -> [Mat4] -> Subgroup
fromMatrices name n minusOne gens = Subgroup
  { sgName     = name
  , sgLevel    = n
  , sgKey      = key
  , sgMinusOne = minusOne
  , sgOrderH   = Just (length hs)
  , sgExpect   = Nothing
  , sgVerdict  = Nothing
  }
  where
    hs = closure n (((n - 1) `mod` n, 0, 0, (n - 1) `mod` n) : [ (a `mod` n, b `mod` n, c `mod` n, d `mod` n) | (a, b, c, d) <- gens ])
    key g =
      let (a0, b0, c0, d0) = entries g
          r x = fromInteger (x `mod` toInteger n)
          gbar = (r a0, r b0, r c0, r d0)
          (a, b, c, d) = minimum [ mulMod n h gbar | h <- hs ]
      in Key [a, b, c, d]

-- | |SL₂(ℤ/N)| = N³ ∏_{p|N} (1 − 1/p²).
sl2Order :: Int -> Int
sl2Order n = fromInteger (toInteger n ^ (3 :: Int) * product [ toInteger (p * p - 1) | p <- ps ] `div` product [ toInteger (p * p) | p <- ps ])
  where
    ps = primeFactors n
    primeFactors m = go m 2
      where
        go 1 _ = []
        go k p | p * p > k = [k]
               | k `mod` p == 0 = p : go (strip k p) p
               | otherwise = go k (p + 1)
        strip k p | k `mod` p == 0 = strip (k `div` p) p
                  | otherwise = k

-- | A subgroup given by its coset action: the name of the coset of @g@ is
-- the image of the base coset under @g@. The action is supplied as a
-- function on words, so this module need not know the representation.
fromAction :: String -> Int -> Bool -> (SL2 -> Int) -> Subgroup
fromAction name level minusOne coset = Subgroup
  { sgName     = name
  , sgLevel    = level
  , sgKey      = \g -> Key [coset g]
  , sgMinusOne = minusOne
  , sgOrderH   = Nothing
  , sgExpect   = Nothing
  , sgVerdict  = Nothing
  }
