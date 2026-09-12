-- | The picture in the unit disk.
--
-- The Cayley transform @c(z) = (z − i)/(z + i)@ takes ℍ to the disk,
-- geodesics to arcs of circles orthogonal to the unit circle, and the real
-- line to the circle itself. An ideal point @p ∈ ℚ ∪ {∞}@ goes to
-- @((p² − 1) + i(−2p)) / (p² + 1)@ — a point of the unit circle with
-- rational coordinates — so, as in the half-plane, every side of every
-- triangle is an exact arc: the circle through two boundary points @u, v@
-- orthogonal to the unit circle has centre @(u + v)/(1 + u·v)@ and radius
-- @√(|m|² − 1)@. Only the vertices are approximate, and those come from
-- @acb@ ("Flint.Ball.projectDisk").
--
-- Two tilings are drawn here. The (2,3,∞) tessellation — the classic
-- two-coloured picture — is the tiling by the images of the half-triangle
-- @T₀ = {i, ρ, ∞}@ under PSL₂(ℤ) and their mirror images; only the
-- PSL₂(ℤ)-images are drawn, in one colour, the mirror images being the
-- disk behind them. And the tiling of the disk by Γ-translates of the
-- domain, which is what makes a fundamental domain a fundamental domain:
-- the side pairings generate Γ, so words in them reach every translate.
module Modular.Disk
  ( boundaryPt, diskGeo
  , triangleDisk, halfDisk, sideDisk, edgePointDisk
  , modularTiles, sidePairings, translates
  ) where

import Data.Array ((!))
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Sq
import Flint.Ball
import Flint.SL2
import Flint.Z
import Modular.Domain
import Modular.Geometry (Seg(..), Shape(..), Ideal(..), ideal)

-- | The Cayley image of an ideal point, exact, on the unit circle.
boundaryPt :: QI -> (Rational, Rational)
boundaryPt Inf     = (1, 0)
boundaryPt (Fin p) = ((p * p - 1) / (p * p + 1), (-2 * p) / (p * p + 1))

onScreen :: DiskView -> (Rational, Rational) -> Screen
onScreen dv (x, y) = (dvCx dv + dvR dv * fromRational x, dvCy dv - dvR dv * fromRational y)

-- | The arc of the geodesic with ideal endpoints @u, v@, from one screen
-- point to another. The sweep is read off the centre: the minor arc from
-- @A@ to @B@ is clockwise on screen iff @(A − M) × (B − M) > 0@.
diskGeo :: DiskView -> QI -> QI -> Screen -> Screen -> Seg
diskGeo dv u v a@(ax, ay) b@(bx, by)
  | ux + vx == 0 && uy + vy == 0 = LineTo b            -- antipodal: a diameter
  | r2 <= 0 || rpx > 1.0e7       = LineTo b
  | otherwise                    = ArcTo rpx sweep b
  where
    (ux, uy) = boundaryPt u
    (vx, vy) = boundaryPt v
    k        = 1 + ux * vx + uy * vy
    m        = ((ux + vx) / k, (uy + vy) / k)
    r2       = fst m * fst m + snd m * snd m - 1
    rpx      = dvR dv * sqrt (fromRational r2)
    (mx, my) = onScreen dv m
    sweep    = (ax - mx) * (by - my) - (ay - my) * (bx - mx) > 0
    _        = a

-- | The triangle @g·F@ in the disk: ρ → ρ+1 → cusp → ρ. No truncation is
-- needed: the cusp is a point of the circle.
triangleDisk :: Prec -> DiskView -> SL2 -> Shape
triangleDisk p dv g = Shape vRho [ diskGeo dv pA pB vRho vRho1, diskGeo dv pR pInf vRho1 cp, diskGeo dv pInf pL cp vRho ] (Just cp)
  where
    (vRho, vRho1) = case projectDisk p dv g [Rho, Rho1] of
      [x, y] -> (x, y)
      _      -> error "projectDisk"
    Ideal pL pR pInf pA pB = ideal g
    cp = onScreen dv (boundaryPt pInf)

-- | One side of @g·F@, as the arc from its start vertex to its end vertex
-- (S: ρ → ρ+1, T: ρ+1 → cusp, T⁻¹: cusp → ρ).
sideDisk :: Prec -> DiskView -> SL2 -> Gen -> (Screen, Seg)
sideDisk p dv g gen = case gen of
  S    -> (vRho,  diskGeo dv pA pB vRho vRho1)
  T    -> (vRho1, diskGeo dv pR pInf vRho1 cp)
  Tinv -> (cp,    diskGeo dv pInf pL cp vRho)
  where
    (vRho, vRho1) = case projectDisk p dv g [Rho, Rho1] of
      [x, y] -> (x, y)
      _      -> error "projectDisk"
    Ideal pL pR pInf pA pB = ideal g
    cp = onScreen dv (boundaryPt pInf)

-- | The left half @g·{i, ρ, ∞}@ of the triangle: i → cusp → ρ → i. Its
-- sides lie on the geodesics (0, ∞), (∞, −½) and (−1, 1).
halfDisk :: Prec -> DiskView -> SL2 -> (Shape, Double)
halfDisk p dv g = (Shape vI [ diskGeo dv p0 pInf vI cp, diskGeo dv pInf pL cp vRho, diskGeo dv pA pB vRho vI ] Nothing, size)
  where
    (vI, vRho) = case projectDisk p dv g [PtI, Rho] of
      [x, y] -> (x, y)
      _      -> error "projectDisk"
    Ideal pL _ pInf pA pB = ideal g
    p0   = act g (Fin 0)
    cp   = onScreen dv (boundaryPt pInf)
    size = maximum [ dist vI cp, dist vRho cp, dist vI vRho ]

dist :: Screen -> Screen -> Double
dist (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int))

-- | A point on the side named by a generator, for markers and curves.
edgePointDisk :: Prec -> DiskView -> SL2 -> Gen -> Screen
edgePointDisk p dv g gen = case projectDisk p dv g [base] of
  [x] -> x
  _   -> error "projectDisk"
  where
    base = case gen of
      T    -> EdgeR
      Tinv -> EdgeL
      S    -> PtI

-- | The half-triangles @γ·T₀@, γ ∈ PSL₂(ℤ), that are at least @minPx@
-- across, found breadth-first from the identity and expanding only from
-- accepted tiles; at most @cap@ of them.
modularTiles :: Prec -> DiskView -> Double -> Int -> [Shape]
modularTiles p dv minPx cap = go (M.singleton identity ()) (Sq.singleton identity) []
  where
    go seen queue acc = case Sq.viewl queue of
      Sq.EmptyL -> reverse acc
      g Sq.:< rest
        | length acc >= cap -> reverse acc
        | otherwise ->
            let (shape, size) = halfDisk p dv g
            in if size < minPx then go seen rest acc else
                 let nexts = [ h | s <- [genT, genTinv, genS], let h = g `mul` s, not (M.member h seen) ]
                     seen' = foldr (\h -> M.insert h ()) seen nexts
                 in go seen' (foldl (Sq.|>) rest nexts) (shape : acc)

-- | The side-pairing elements of a domain: for every unglued side @(i, g)@
-- paired with triangle @j@, the element @reps[i]·g·reps[j]⁻¹ ∈ Γ@ that
-- carries @j@'s triangle onto the position across that side. They
-- generate Γ.
sidePairings :: Domain -> [SL2]
sidePairings dom = M.keys $ M.fromList
  [ (γ, ()) | i <- [0 .. size dom - 1], g <- gens, let e = edge dom i g, not (eGlued e)
            , let γ = ((reps ! i) `mul` genMat g) `mul` inv (reps ! eNbr e), γ /= identity ]
  where reps = dReps dom

-- | Γ-translates @γ·D@ with @γ·F@ at least @minPx@ across, as words in the
-- side pairings, breadth-first; each with its word length, whose parity
-- gives the two colours. At most @cap@ of them.
translates :: Prec -> DiskView -> Domain -> Double -> Int -> [(Int, SL2)]
translates p dv dom minPx cap = go (M.singleton identity 0) (Sq.singleton (0, identity)) []
  where
    pairings = sidePairings dom
    go seen queue acc = case Sq.viewl queue of
      Sq.EmptyL -> reverse acc
      (d, γ) Sq.:< rest
        | length acc >= cap -> reverse acc
        | otherwise ->
            let sh = triangleDisk p dv γ
                pts = shStart sh : [ q | seg <- shSegs sh, let q = case seg of { ArcTo _ _ e -> e; LineTo e -> e } ]
                size = maximum [ dist a b | a <- pts, b <- pts ]
            in if size < minPx && d > 0 then go seen rest acc else
                 let nexts = [ (d + 1, h) | s <- pairings, let h = γ `mul` s, not (M.member h seen) ]
                     seen' = foldr (\(_, h) -> M.insert h (d + 1)) seen nexts
                 in go seen' (foldl (Sq.|>) rest nexts) ((d, γ) : acc)
