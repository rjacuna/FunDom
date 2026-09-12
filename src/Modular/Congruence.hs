-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | Is a finite-index subgroup of PSL₂(ℤ) a congruence subgroup?
--
-- Hsu's criterion (T. Hsu, "Identifying congruence subgroups of the
-- modular group", Proc. AMS 124 (1996)): with @L = [1 1; 0 1]@ and
-- @R = [1 0; 1 1]@ acting on the cosets, and @N@ the generalised level
-- (the lcm of the cusp widths, which for a congruence subgroup is the
-- level, by Wohlfahrt), the subgroup contains Γ(N) iff a short list of
-- relations — a presentation of PSL₂(ℤ/N) — holds among the permutations.
-- The relations are transcribed from Sage's @is_congruence@, which follows
-- Hsu and Kurth's KFarey; words are read left to right, acting on the
-- right, as everywhere in this program.
--
-- 'bruteForce' is the definition itself — the orbit of the pair
-- (base coset, identity mod N) has size @[PSL₂(ℤ) : Γ ∩ Γ(N)]@, which
-- equals @|PSL₂(ℤ/N)|@ iff @Γ ⊇ Γ(N)@ — and is what the test-suite checks
-- the criterion against on small levels.
module Modular.Congruence
  ( Verdict(..), certify, bruteForce, psl2Order
  ) where

import Data.Array.Unboxed
import qualified Data.Set as Set
import Modular.Coset
import Modular.Group (sl2Order)

data Verdict
  = Congruence
  | NotCongruence String    -- ^ which of Hsu's relations fails
  deriving (Eq, Show)

type Perm = UArray Int Int

-- Permutations, composed left to right ---------------------------------------------

pmul :: Perm -> Perm -> Perm
pmul a b = amap (b !) a

pinv :: Perm -> Perm
pinv p = array (bounds p) [ (p ! i, i) | i <- indices p ]

pid :: Perm -> Perm
pid p = listArray (bounds p) (indices p)

ppow :: Perm -> Int -> Perm
ppow p k
  | k < 0     = ppow (pinv p) (negate k)
  | k == 0    = pid p
  | even k    = let h = ppow p (k `div` 2) in pmul h h
  | otherwise = pmul p (ppow p (k - 1))

isOne :: Perm -> Bool
isOne p = all (\i -> p ! i == i) (indices p)

prod :: [Perm] -> Perm
prod = foldl1 pmul

-- | Modular inverse by extended Euclid.
invMod :: Int -> Int -> Int
invMod a m = let (g, x, _) = egcd (a `mod` m) m in if g /= 1 then error "invMod" else x `mod` m
  where
    egcd 0 b = (b, 0, 1)
    egcd a' b = let (g, x, y) = egcd (b `mod` a') a' in (g, y - (b `div` a') * x, x)

-- | The generalised level and the verdict.
certify :: PermRep -> (Int, Verdict)
certify pr
  | prSize pr == 1 = (1, Congruence)
  | e == 1 = (n, check [ ("Hsu's relation for odd level", prod [r, r, ppow l (negate (invMod 2 n)), r, r, ppow l (negate (invMod 2 n)), r, r, ppow l (negate (invMod 2 n))]) ])
  | m == 1 =
      let s = prod [ppow l 20, ppow r (invMod 5 n), ppow l (-4), pinv r]
      in (n, check
           [ ("A1", prod [pinv l, r, pinv l, s, l, pinv r, l, s])
           , ("A2", prod [pinv s, r, s, ppow r (-25)])
           , ("A3", pmul (ppow (prod [s, ppow r 5, l, pinv r, l]) 3) (pinv (ppow (prod [l, pinv r, l]) 2)))
           ])
  | otherwise =
      let c  = (e * invMod e m) `mod` n          -- c ≡ 0 (e), c ≡ 1 (m)
          d  = (m * invMod m e) `mod` n          -- d ≡ 1 (e), d ≡ 0 (m)
          a  = ppow l c
          b  = ppow r c
          l' = ppow l d
          r' = ppow r d
          s  = prod [ppow l' 20, ppow r' (invMod 5 e), ppow l' (-4), pinv r']
          half = invMod 2 m
      in (n, check
           [ ("B1", prod [pinv a, pinv r', a, r'])
           , ("B2", ppow (prod [a, pinv b, a]) 4)
           , ("B3", pmul (ppow (prod [a, pinv b, a]) 2) (ppow (pmul (pinv a) b) 3))
           , ("B4", pmul (ppow (prod [a, pinv b, a]) 2) (ppow (prod [b, b, ppow a (negate half)]) (-3)))
           , ("B5", prod [pinv l', r', pinv l', s, l', pinv r', l', s])
           , ("B6", prod [pinv s, r', s, ppow r' (-25)])
           , ("B7", pmul (ppow (prod [l', pinv r', l']) 2) (ppow (prod [s, ppow r' 5, l', pinv r', l']) (-3)))
           ])
  where
    l = permL pr
    r = permR pr
    n = generalisedLevel pr
    m = oddPart n
    e = n `div` m
    oddPart k | even k = oddPart (k `div` 2)
              | otherwise = k
    check rels = case [ name | (name, p) <- rels, not (isOne p) ] of
      []         -> Congruence
      (name : _) -> NotCongruence name

-- | |PSL₂(ℤ/N)|.
psl2Order :: Int -> Int
psl2Order 1 = 1
psl2Order 2 = 6
psl2Order n = sl2Order n `div` 2

-- | Γ ⊇ Γ(N), from the definition: the orbit of (base coset, I mod N)
-- under the diagonal action has size [PSL₂(ℤ) : Γ ∩ Γ(N)].
bruteForce :: PermRep -> Int -> Bool
bruteForce pr n = orbit == psl2Order n
  where
    orbit = go (Set.singleton start) [start]
    start = (0, canon (1, 0, 0, 1))
    go seen [] = Set.size seen
    go seen (x : queue) =
      let nexts = [ y | y <- [stepS x, stepR x], not (Set.member y seen) ]
      in go (foldr Set.insert seen nexts) (queue ++ nexts)
    stepS (c, mt) = (prS pr ! c, canon (mulN mt (0, n - 1, 1, 0)))
    stepR (c, mt) = (prR pr ! c, canon (mulN mt (0, n - 1, 1, 1)))
    mulN (a, b, c, d) (e', f, g, h) =
      ((a * e' + b * g) `mod` n, (a * f + b * h) `mod` n, (c * e' + d * g) `mod` n, (c * f + d * h) `mod` n)
    canon mt@(a, b, c, d) = min mt (((n - a) `mod` n, (n - b) `mod` n, (n - c) `mod` n, (n - d) `mod` n))
