{-# LANGUAGE TupleSections #-}
-- | Checks against closed formulas, table consistency, and the two FLINT
-- matrix paths against each other. Exits nonzero on any failure.
module Main (main) where

import Control.Monad (unless)
import Data.Array ((!))
import Data.Array.Unboxed (listArray)
import Data.List (nub, sort)
import Data.Ratio
import Flint.Ball
import Flint.Mat
import Flint.SL2
import Flint.Z
import Modular.Congruence
import Modular.Coset
import Modular.CP
import Modular.Disk
import Modular.Domain
import Modular.Generators
import Modular.Word
import qualified Data.Map.Strict as M
import Modular.Geometry
import Modular.Group
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import Web.Params

-- Number theory for the formulas ------------------------------------------------

primes :: Int -> [Int]
primes n = nub [ p | p <- factors n, p > 1, all (\q -> p `mod` q /= 0) [2 .. p - 1] ]
  where factors m = [ k | k <- [1 .. m], m `mod` k == 0 ]

divisors :: Int -> [Int]
divisors n = [ k | k <- [1 .. n], n `mod` k == 0 ]

phi :: Int -> Int
phi n = length [ k | k <- [1 .. n], gcd k n == 1 ]

-- Γ₀(N): μ = N∏(1+1/p); cusps Σ_{d|N} φ(gcd(d,N/d)); e2, e3 by Legendre symbols
mu0, cusps0, e2_0, e3_0, genus0 :: Int -> Int
mu0 n = n * product [ p + 1 | p <- primes n ] `div` product (primes n)
cusps0 n = sum [ phi (gcd d (n `div` d)) | d <- divisors n ]
e2_0 n | n `mod` 4 == 0 = 0
       | otherwise = product [ 1 + chi p | p <- primes n ]
  where chi p | p == 2 = 0 | p `mod` 4 == 1 = 1 | otherwise = -1
e3_0 n | n `mod` 9 == 0 = 0
       | otherwise = product [ 1 + chi p | p <- primes n ]
  where chi p | p == 3 = 0 | p `mod` 3 == 1 = 1 | otherwise = -1
genus0 n = 1 + (mu0 n - 3 * e2_0 n - 4 * e3_0 n - 6 * cusps0 n) `div` 12

-- Γ₁(N), projective
mu1, cusps1, e2_1, e3_1 :: Int -> Int
mu1 n | n <= 2 = mu0 n
      | otherwise = (n * n * product [ p * p - 1 | p <- primes n ]) `div` (2 * product [ p * p | p <- primes n ])
cusps1 n | n == 1 = 1 | n == 2 = 2 | n == 3 = 2 | n == 4 = 3
         | otherwise = sum [ phi d * phi (n `div` d) | d <- divisors n ] `div` 2
e2_1 n | n <= 2 = 1 | otherwise = 0
e3_1 n | n == 1 || n == 3 = 1 | otherwise = 0

-- Γ(N), projective
muF, cuspsF :: Int -> Int
muF n | n == 1 = 1 | n == 2 = 6
      | otherwise = (n * n * n * product [ p * p - 1 | p <- primes n ]) `div` (2 * product [ p * p | p <- primes n ])
cuspsF n | n == 1 = 1 | n == 2 = 3 | otherwise = muF n `div` n

genusOf :: Int -> Int -> Int -> Int -> Int
genusOf mu e2 e3 c = 1 + (mu - 3 * e2 - 4 * e3 - 6 * c) `div` 12

-- The checks ------------------------------------------------------------------------

dom :: GroupType -> Int -> GroupType -> Int -> Domain
dom t1 n t2 m = either error id (enumerate (subgroup t1 n t2 m))

invariants :: Domain -> (Int, Int, Int, Int, Int)
invariants d = let i = info d in (iIndex i, length (iCusps i), iE2 i, iE3 i, iGenus i)

checkFormulas :: [(String, Bool)]
checkFormulas = concat
  [ [ ("Γ₀(" ++ show n ++ ")", invariants (dom G0 n G0 1) == (mu0 n, cusps0 n, e2_0 n, e3_0 n, genus0 n)) | n <- [1 .. 60] ]
  , [ ("Γ⁰(" ++ show n ++ ")", invariants (dom Gup0 n G0 1) == (mu0 n, cusps0 n, e2_0 n, e3_0 n, genus0 n)) | n <- [1 .. 40] ]
  , [ ("Γ₁(" ++ show n ++ ")", invariants (dom G1 n G0 1) == (mu1 n, cusps1 n, e2_1 n, e3_1 n, genusOf (mu1 n) (e2_1 n) (e3_1 n) (cusps1 n))) | n <- [1 .. 30] ]
  , [ ("Γ¹(" ++ show n ++ ")", invariants (dom Gup1 n G0 1) == (mu1 n, cusps1 n, e2_1 n, e3_1 n, genusOf (mu1 n) (e2_1 n) (e3_1 n) (cusps1 n))) | n <- [1 .. 25] ]
  , [ ("Γ(" ++ show n ++ ")", invariants (dom Gfull n G0 1) == (muF n, cuspsF n, ee, ee, genusOf (muF n) ee ee (cuspsF n)))
    | n <- [1 .. 13], let ee = if n == 1 then 1 else 0 ]
    -- the classical genera
  , [ ("g(X₀(11)) = 1", genusOf' (dom G0 11 G0 1) == 1), ("g(X₀(37)) = 2", genusOf' (dom G0 37 G0 1) == 2)
    , ("g(X₁(13)) = 2", genusOf' (dom G1 13 G0 1) == 2), ("g(X(7)) = 3", genusOf' (dom Gfull 7 G0 1) == 3)
    , ("g(X(11)) = 26", genusOf' (dom Gfull 11 G0 1) == 26) ]
    -- intersections against what they equal
  , [ ("Γ₀(" ++ show n ++ ") ∩ Γ₀(" ++ show m ++ ") = Γ₀(lcm)", invariants (dom G0 n G0 m) == invariants (dom G0 (lcm n m) G0 1))
    | (n, m) <- [(4, 6), (9, 12), (5, 7), (8, 12)] ]
  , [ ("Γ₀(N) ∩ Γ⁰(N) ~ Γ₀(N²), N=" ++ show n, invariants (dom G0 n Gup0 n) == invariants (dom G0 (n * n) G0 1)) | n <- [2, 3, 5] ]
  , [ ("Γ₁(N) ∩ Γ₀(M) index, N=" ++ show n ++ " M=" ++ show m, iIndex (info (dom G1 n G0 m)) == mu0 m * phi n `div` 2)
    | (n, m) <- [(5, 10), (7, 14), (3, 9)] ]
  ]
  where genusOf' = iGenus . info

-- the coset table: inverse edges match, glued sides are adjacent, keys are distinct
checkTable :: String -> Domain -> [(String, Bool)]
checkTable name d =
  [ (name ++ ": edges invert",   and [ neighbour d (neighbour d i g) (genInverse g) == i | i <- idx, g <- gens ])
  , (name ++ ": glued = adjacent", and [ eGlued (edge d i g) == ((reps ! i `mul` genMat g) == reps ! eNbr (edge d i g)) | i <- idx, g <- gens ])
  , (name ++ ": neighbour is the coset", and [ key (reps ! i `mul` genMat g) == key (reps ! neighbour d i g) | i <- idx, g <- gens ])
  , (name ++ ": keys distinct", length (nub (map (key . (reps !)) idx)) == size d)
  , (name ++ ": Euler integral", iEuler (info d))
  , (name ++ ": one region", connected d)
  ]
  where
    reps = dReps d
    idx  = [0 .. size d - 1]
    key  = cosetKey (dGroup d)

-- moves keep the identification and restore the glued-means-adjacent invariant
checkMoves :: [(String, Bool)]
checkMoves =
  [ ("move keeps cosets",    and [ neighbour d' i g == neighbour d0 i g | i <- [0 .. size d0 - 1], g <- gens ])
  , ("move keeps invariants", invariants d' == invariants d0)
  , ("move: glued = adjacent", all snd (checkTable "moved" d'))
  , ("move changed something", dReps d' /= dReps d0)
  , ("move keeps one region",  connected d')
  , ("Γ₀(11) mv=3S is one region", connected (applyMoves [(3, S)] d0))
  , ("Γ₀(11) mv=3S moves the block by T", dReps (applyMoves [(3, S)] d0) ! 4 == genT `mul` (dReps d0 ! 4))
  , ("every single move keeps one region", and [ connected (applyMoves [m] d1) | m <- unglueds d1 ])
  , ("a chain of moves keeps one region", connected (foldl (\d m -> applyMoves [m] d) d1 (take 12 (cycle (unglueds d1)))))
  , ("self-paired side is left alone", applyMoves [(0, T)] d0 `sameAs` d0)
  ]
  where
    d0 = dom G0 11 G0 1
    d1 = dom G0 30 G0 1
    unglueds d = [ (i, g) | i <- [0 .. size d - 1], g <- gens, not (eGlued (edge d i g)), neighbour d i g /= i ]
    unglued = head (unglueds d0)
    d' = applyMoves [unglued] d0
    sameAs a b = dReps a == dReps b && dEdges a == dEdges b

checkSL2 :: [(String, Bool)]
checkSL2 =
  [ ("S² = 1",        genS `mul` genS == identity)
  , ("(ST)³ = 1",     let r = genS `mul` genT in r `mul` r `mul` r == identity)
  , ("R = ST",        genR == genS `mul` genT)
  , ("inverse",       all (\g -> g `mul` inv g == identity) ws)
  , ("sign is dropped", sl2 (-1) 0 0 (-1) == Just identity && sl2 0 1 (-1) 0 == Just genS)
  , ("det 2 refused", sl2 2 0 0 1 == Nothing)
  , ("S·2 = −1/2",    act genS (Fin 2) == Fin (-1 % 2))
  , ("T·∞ = ∞",       act genT Inf == Inf)
  , ("[1 2;3 7]·∞ = 1/3", act (sl2' 1 2 3 7) Inf == Fin (1 % 3))
  , ("S·0 = ∞",       act genS (Fin 0) == Inf)
  , ("mat path agrees with psl2z path",
       and [ toSL2 (either error id (maybe (Left "dim") Right (zmul (fromSL2 x) (fromSL2 y)))) == Just (x `mul` y) | x <- ws, y <- ws ])
  , ("det 1 on the fmpz_mat side", all (\g -> zdet (fromSL2 g) == Just 1) ws)
  ]
  where
    ws = take 40 (iterate (\g -> g `mul` genT `mul` genS) identity) ++ [sl2' 5 (-7) 3 (-4), sl2' 1 100 0 1, sl2' 100 1 99 1]

checkMat :: [(String, Bool)]
checkMat =
  [ ("zinv",   (zinv =<< zmat [[2, 1], [1, 1]]) == fmap (\m -> (m, 1)) (zmat [[1, -1], [-1, 2]]))
  , ("zinv singular", (zinv =<< zmat [[1, 2], [2, 4]]) == Nothing)
  , ("zsolve: A·X = den·B", case (do a <- zmat [[2, 1], [0, 4]]; b <- zmat [[1], [1]]; (a, b,) <$> zsolve a b) of
                              Just (a, b, (x, den)) -> zmul a x == zmat [ map (* den) row | row <- zlist b ]
                              Nothing -> False)
  , ("zrref rank", let Just m = zmat [[1, 2, 3], [2, 4, 6], [1, 0, 1]]; (_, _, r) = zrref m in r == 2)
  , ("znullspace", let Just m = zmat [[1, 1, 1]]
                       ns = znullspace m
                   in length ns == 2 && all (\v -> sum v == 0) ns)
  , ("qinv",   (qinv =<< qmat [[1, 2], [3, 4]]) == qmat [[-2, 1], [3 % 2, -1 % 2]])
  , ("qsolve", (do a <- qmat [[1, 1], [1, -1]]; b <- qmat [[3], [1]]; qsolve a b) == qmat [[2], [1]])
  , ("qdet",   (qdet =<< qmat [[1 % 2, 1], [1, 1 % 2]]) == Just (-3 % 4))
  , ("qrref",  fmap qrref (qmat [[2, 4], [1, 2]]) == fmap (\m -> (m, 1)) (qmat [[1, 2], [0, 0]]))
  ]

checkGeometry :: [(String, Bool)]
checkGeometry =
  [ ("ρ on screen", close (head (project prec v identity [Rho])) (425, 460 - 25 * sqrt 3))
  , ("S(i) = i",    close (head (project prec v genS [PtI])) (450, 410))
  , ("T·ρ",         close (head (project prec v genT [Rho])) (475, 460 - 25 * sqrt 3))
  , ("unit circle is the bottom of F", geodesic (Fin (-1)) (Fin 1) == Just (Semi 0 1))
  , ("left side of F is vertical",     geodesic Inf (Fin (-1 % 2)) == Just (Vert (-1 % 2)))
  , ("S·F sits on the arcs −2↔0 and 0↔2", let Ideal l r i _ _ = ideal genS in (l, r, i) == (Fin 2, Fin (-2), Fin 0))
  , ("extent of F", xExtent identity == (-1, 1))
  , ("cusp of triangle F is ∞ → no cusp point", case shCusp (triangleShape prec v identity) of Nothing -> True; _ -> False)
  , ("cusp of S·F at 0", case shCusp (triangleShape prec v genS) of Just c' -> close c' (450, 460); _ -> False)
  ]
  where
    v = View { vW = 900, vH = 520, vY0 = 60, vScale = 50, vCx = 0 }
    prec = precFor v
    close (x1, y1) (x2, y2) = abs (x1 - x2) < 1e-6 && abs (y1 - y2) < 1e-6

checkDisk :: [(String, Bool)]
checkDisk =
  [ ("∞ ↦ 1",        boundaryPt Inf == (1, 0))
  , ("0 ↦ −1",       boundaryPt (Fin 0) == (-1, 0))
  , ("1 ↦ −i",       boundaryPt (Fin 1) == (0, -1))
  , ("−1 ↦ i",       boundaryPt (Fin (-1)) == (0, 1))
  , ("i ↦ centre",   close (head (projectDisk 96 dv identity [PtI])) (300, 300))
  , ("ρ ↦ Cayley(ρ)", close (head (projectDisk 96 dv identity [Rho])) (300 + 200 * rx, 300 - 200 * ry))
  , ("imaginary axis is a diameter", case diskGeo dv (Fin 0) Inf (100, 300) (500, 300) of { LineTo _ -> True; _ -> False })
  , ("unit circle ↦ the vertical diameter", case diskGeo dv (Fin (-1)) (Fin 1) (300, 100) (300, 500) of { LineTo _ -> True; _ -> False })
  , ("geodesic 0↔1 ↦ orthogonal circle of radius 1",
       case diskGeo dv (Fin 0) (Fin 1) (100, 300) (300, 500) of { ArcTo r _ _ -> abs (r - 200) < 1e-6; _ -> False })
  , ("a modular tile is a triangle", length (shSegs (fst (halfDisk 96 dv identity))) == 3)
  , ("tessellation grows", length (modularTiles 96 dv 3 5000) > 100)
  , ("side pairings of Γ₀(11) lie in Γ₀(11)", all (\g -> let (_, _, c', _) = entries g in c' `mod` 11 == 0) (sidePairings (dom G0 11 G0 1)))
  , ("translates of Γ₀(11) lie in Γ₀(11)", all (\(_, g) -> let (_, _, c', _) = entries g in c' `mod` 11 == 0) (translates 96 dv (dom G0 11 G0 1) 4 200))
  , ("translates are distinct", let ts = map snd (translates 96 dv (dom G0 11 G0 1) 4 200) in length (nub ts) == length ts && length ts > 20)
  ]
  where
    dv = DiskView 300 300 200
    -- c(ρ) = (ρ − i)/(ρ + i) with ρ = −½ + i√3/2: numerically
    (rx, ry) = let (a, b) = (-0.5, sqrt 3 / 2)
                   (nr, ni) = (a, b - 1)
                   (dr, di) = (a, b + 1)
                   den = dr * dr + di * di
               in ((nr * dr + ni * di) / den, (ni * dr - nr * di) / den)
    close (x1, y1) (x2, y2) = abs (x1 - x2) < 1e-6 && abs (y1 - y2) < 1e-6

-- Words, coset enumeration from generators, and the congruence test ---------------

-- a small deterministic generator for random permutation representations
lcg :: Int -> Int
lcg x = (x * 1103515245 + 12345) `mod` 2147483648

-- a random transitive action of S (involution) and R (order 3) on n points
randomRep :: Int -> Int -> Maybe PermRep
randomRep seed n =
  let (s1, sPerm) = involution seed
      (_,  rPerm) = order3 s1
      pr = PermRep n (listArray (0, n - 1) sPerm) (listArray (0, n - 1) rPerm)
  in if transitive pr then Just pr else Nothing
  where
    -- pair up points at random into 2-cycles, leaving some fixed
    involution sd = go sd [0 .. n - 1] (M.empty)
      where
        go x [] m = (x, [ M.findWithDefault i i m | i <- [0 .. n - 1] ])
        go x (i : rest) m
          | M.member i m = go x rest m
          | otherwise = let x' = lcg x
                            free = [ j | j <- rest, not (M.member j m) ]
                        in if null free || x' `mod` 3 == 0 then go x' rest (M.insert i i m)
                           else let j = free !! (x' `mod` length free) in go x' rest (M.insert i j (M.insert j i m))
    order3 sd = go sd [0 .. n - 1] M.empty
      where
        go x [] m = (x, [ M.findWithDefault i i m | i <- [0 .. n - 1] ])
        go x (i : rest) m
          | M.member i m = go x rest m
          | otherwise = let x' = lcg x
                            free = [ j | j <- rest, not (M.member j m) ]
                        in if length free < 2 || x' `mod` 4 == 0 then go x' rest (M.insert i i m)
                           else let j = free !! (x' `mod` length free)
                                    free' = filter (/= j) free
                                    k = free' !! (lcg x' `mod` length free')
                                in go (lcg x') rest (M.insert i j (M.insert j k (M.insert k i m)))
    transitive pr = length (reach' pr [0] [0]) == n
    reach' _ seen [] = seen
    reach' pr seen (c : queue) =
      let nexts = nub [ d | d <- [actLetter pr LS c, actLetter pr LR c], d `notElem` seen ]
      in reach' pr (seen ++ nexts) (queue ++ nexts)

checkWords :: [(String, Bool)]
checkWords =
  [ ("word round trip", all (\g -> wordMat (wordOf g) == g) ws)
  , ("words are reduced", all (\g -> reduceW (wordOf g) == wordOf g) ws)
  , ("T = SR", wordOf genT == [LS, LR] && wordOf genTinv == [LRi, LS])
  , ("S", wordOf genS == [LS])
  , ("R", wordOf genR == [LR] && wordOf (inv genR) == [LRi])
  ]
  where ws = take 60 (iterate (\g -> g `mul` genT `mul` genS) identity) ++ [sl2' 5 (-7) 3 (-4), sl2' 1 100 0 1, sl2' 100 1 99 1, sl2' 7 (-11) (-19) 30]

-- the side pairings of a domain generate the group: enumerate from them
checkEnumeration :: [(String, Bool)]
checkEnumeration =
  [ (name ++ ": side pairings give the index", case enumerateMatrices (sidePairings d) of
        Finite pr -> prSize pr == size d
        Infinite _ -> False)
  | (name, d) <- groups ] ++
  [ (name ++ ": congruence, level " ++ show lv, case enumerateMatrices (sidePairings d) of
        Finite pr -> certify pr == (lv, Congruence)
        Infinite _ -> False)
  | (name, d, lv) <- levelled ] ++
  [ ("SL₂(ℤ) itself", case enumerateMatrices [genS, genT] of { Finite pr -> prSize pr == 1 && certify pr == (1, Congruence); _ -> False })
  , ("⟨S⟩ has infinite index", case enumerateMatrices [genS] of { Infinite _ -> True; _ -> False })
  , ("⟨T⟩ has infinite index", case enumerateMatrices [genT] of { Infinite _ -> True; _ -> False })
  , ("⟨T², ST²S⟩ = Γ(2)", case enumerateMatrices [genT `mul` genT, genS `mul` genT `mul` genT `mul` genS] of
        Finite pr -> prSize pr == 6 && certify pr == (2, Congruence); _ -> False)
  , ("⟨T, ST²S⟩ = Γ₀(2)", case enumerateMatrices [genT, genS `mul` genT `mul` genT `mul` genS] of
        Finite pr -> prSize pr == 3 && certify pr == (2, Congruence); _ -> False)
  ]
  where
    groups   = [ (n, dom t k G0 1) | (n, t, k) <- [("Γ₀(11)", G0, 11), ("Γ₀(30)", G0, 30), ("Γ₁(13)", G1, 13), ("Γ(5)", Gfull, 5), ("Γ(7)", Gfull, 7), ("Γ⁰(6)", Gup0, 6), ("Γ¹(8)", Gup1, 8)] ]
               ++ [ ("Γ₀(6)∩Γ₁(4)", dom G0 6 G1 4) ]
    levelled = [ ("Γ₀(" ++ show k ++ ")", dom G0 k G0 1, k) | k <- [1 .. 30] ]
            ++ [ ("Γ₁(" ++ show k ++ ")", dom G1 k G0 1, k) | k <- [3 .. 13] ]
            ++ [ ("Γ(" ++ show k ++ ")", dom Gfull k G0 1, k) | k <- [2 .. 8] ]
            ++ [ ("Γ⁰(" ++ show k ++ ")", dom Gup0 k G0 1, k) | k <- [4, 9, 12] ]

-- Hsu's criterion against the definition, on random actions of small level,
-- and the enumeration round-tripped through Schreier generators
checkCongruence :: [(String, Bool)]
checkCongruence =
  [ ("random reps: Hsu agrees with brute force (" ++ show (length small) ++ " cases, "
        ++ show (length [ () | (_, _, v) <- small, v == Congruence ]) ++ " congruence)",
       and [ (v == Congruence) == bruteForce pr lv | (pr, lv, v) <- small ])
  , ("random reps: some are not congruence", any (\(_, _, v) -> v /= Congruence) small)
  , ("random reps: some are congruence", any (\(_, _, v) -> v == Congruence) small)
  , ("Schreier generators re-enumerate to the same index", and
       [ case enumerateMatrices (schreierGenerators pr) of { Finite pr' -> prSize pr' == prSize pr; _ -> False } | (pr, _, _) <- take 40 reps ])
  , ("Schreier generators keep the level and verdict", and
       [ case enumerateMatrices (schreierGenerators pr) of { Finite pr' -> certify pr' == (lv, v); _ -> False } | (pr, lv, v) <- take 40 reps ])
  ]
  where
    reps  = [ (pr, lv, v) | seed <- [1 .. 400], n <- [2 .. 9], Just pr <- [randomRep (seed * 7919 + n) n], let (lv, v) = certify pr ]
    small = [ x | x@(_, lv, _) <- reps, lv <= 24 ]

-- Mode 3 end to end, on the worked examples
checkGenerators :: [(String, Bool)]
checkGenerators =
  [ ("parse", fmap length (parseGenerators "1 1 0 1\n1 0 2 1") == Right 2)
  , ("parse: determinant", either (const True) (const False) (parseGenerators "1 2 3 4"))
  , ("parse: brackets and commas", fmap length (parseGenerators "[[1,1],[0,1]], [[1,0],[2,1]]") == Right 2)
  , ("Γ₀(2) example", verdictOf "1 1 0 1\n1 0 2 1" == Just (3, 2, Nothing))
  , ("Γ(2) example", verdictOf "1 2 0 1\n1 0 2 1" == Just (6, 2, Nothing))
  , ("Γ₀(11) example: index 12, level 11, congruence", verdictOf (snd (examples !! 2)) == Just (12, 11, Nothing))
  , ("non-congruence example: index 7, not congruence", case verdictOf (snd (examples !! 3)) of { Just (7, _, Just _) -> True; _ -> False })
  , ("generators: domain reproduces the action", and
      [ case parseGenerators g >>= groupFromGenerators of
          Right sg -> case (enumerate sg, sgExpect sg) of
            (Right d, Just ex) -> let i = info d in iIndex i == exIndex ex && iGenus i == exGenus ex
                                     && sort (map cWidth (iCusps i)) == sort (exCusps ex) && iE2 i == exE2 ex && iE3 i == exE3 ex && connected d
            _ -> False
          Left _ -> False
      | (_, g) <- examples ])
  , ("infinite index reported", either (const True) (const False) (parseGenerators "0 -1 1 0" >>= groupFromGenerators))
  ]
  where
    verdictOf txt = case parseGenerators txt >>= groupFromGenerators of
      Right sg | Just (lv, v) <- sgVerdict sg, Just ex <- sgExpect sg -> Just (exIndex ex, lv, v)
      _ -> Nothing

checkParams :: [(String, Bool)]
checkParams =
  [ ("round trip", let p = defaultParams { pG1 = G1, pN = 13, pScale = 7 % 3, pCx = -5 % 2, pEdit = True, pMoves = [(3, T), (0, S)], pSel = Just 4 }
                       p' = parseParams (parseQuery (toQuery p))
                   in (pG1 p', pN p', pScale p', pCx p', pEdit p', pMoves p', pSel p') == (G1, 13, 7 % 3, -5 % 2, True, [(3, T), (0, S)], Just 4))
  , ("readRat", map readRat ["50", "-3/7", "2.25", "x", "1/0"] == [Just 50, Just (-3 % 7), Just (9 % 4), Nothing, Nothing])
  , ("showRat", map showRat [50, 9 % 4, -3 % 7, 1 % 3] == ["50", "2.25", "-3/7", "1/3"])
  , ("mats round trip", readMats (showMats [genS, genT, sl2' 5 (-7) 3 (-4)]) == [genS, genT, sl2' 5 (-7) 3 (-4)])
  , ("explorer boxes", pMats (parseParams [("a", "0"), ("b", "-1"), ("c", "1"), ("d", "0")]) == [genS])
  , ("urlEncode", urlEncode "a b&c=d" == "a%20b%26c%3Dd")
  ]

-- Against the Cummins–Pauli tables, when they are in place: every group of
-- a few levels, enumerated from its generators, must reproduce their index,
-- genus, cusp widths and elliptic points.
checkCP :: IO [(String, Bool)]
checkCP = do
  present <- doesFileExist "csg/csg1-lev8.dat"
  if not present then return [("csg/ tables present (skipped)", True)] else do
    r <- findRecord "csg" "11A1"
    let parsed = case r of
          Right rec -> rLevel rec == 11 && rIndex rec == 12 && rGenus rec == 1 && rMinusOne rec
                       && rMatGens rec == [(7, 0, 7, 8), (8, 1, 6, 5), (10, 0, 0, 10)] && rCusps rec == [1, 11]
                       && rSpecial rec == Just "\\overline\\Gamma_0(11)"
          Left _ -> False
    rt <- case r of
      Right rec -> return (fmap compactRecord (parseCompact (compactRecord rec)) == Right (compactRecord rec)
                           && map compactSummary (parseSummaries (compactSummary (summaryOf rec))) == [compactSummary (summaryOf rec)])
      Left _ -> return False
    lvls <- mapM (\lv -> map ((,) lv) <$> scanLevel "csg" lv) [6, 7, 11, 12]
    rows <- mapM check (concat lvls)
    return (("parse 11A1", parsed) : ("compact formats round-trip", rt) : rows)
  where
    check (lv, sm) = do
      r <- findRecord "csg" (smName sm)
      return $ (,) ("CP " ++ smName sm ++ " (level " ++ show lv ++ ")") $ case r of
        Left _ -> False
        Right rec -> case enumerate (toSubgroup rec) of
          Left _ -> False
          Right d ->
            let i = info d
                e2 = length (filter (== 1) (rC2 rec))
                e3 = length (filter (== 1) (rC3 rec))
            in iIndex i == rIndex rec && iGenus i == rGenus rec && iE2 i == e2 && iE3 i == e3
               && sort (map cWidth (iCusps i)) == sort (rCusps rec) && connected d
               && maybe False (\h -> h * iIndex i == sl2Order (rLevel rec)) (sgOrderH (toSubgroup rec))

main :: IO ()
main = do
  cp <- checkCP
  let tables = concat [ checkTable ("Γ₀(30)") (dom G0 30 G0 1), checkTable "Γ₁(7)" (dom G1 7 G0 1)
                      , checkTable "Γ(5)" (dom Gfull 5 G0 1), checkTable "Γ⁰(6)∩Γ₁(4)" (dom Gup0 6 G1 4) ]
      results = checkSL2 ++ checkMat ++ checkFormulas ++ tables ++ checkMoves ++ checkGeometry ++ checkDisk ++ checkParams
                ++ checkWords ++ checkEnumeration ++ checkCongruence ++ checkGenerators ++ cp
      failed = [ n | (n, False) <- results ]
  mapM_ (\(n, ok) -> putStrLn ((if ok then "ok    " else "FAIL  ") ++ n)) results
  case [ pr | (pr, _, NotCongruence _) <- take 2000 [ (pr, lv, v) | seed <- [1 .. 60], n <- [7 .. 7], Just pr <- [randomRep (seed * 7919 + n) n], let (lv, v) = certify pr ] ] of
    (pr : _) -> putStrLn ("note: a non-congruence subgroup of index " ++ show (prSize pr) ++ ": " ++ show (map entries (schreierGenerators pr)))
    [] -> return ()
  putStrLn ("note: shipped non-congruence example gives " ++ show (fmap (\sg -> (sgVerdict sg, fmap exIndex (sgExpect sg))) (parseGenerators (snd (examples !! 3)) >>= groupFromGenerators)))
  putStrLn (show (length results - length failed) ++ " / " ++ show (length results) ++ " passed")
  unless (null failed) exitFailure
