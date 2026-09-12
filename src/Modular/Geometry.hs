-- | From a matrix to the shape on the screen.
--
-- The sides of @g·F@ are geodesics, and a geodesic is determined by its two
-- ideal endpoints in ℝ ∪ {∞}. The endpoints of the sides of @F@ are
-- −½, ½, ∞ (the two vertical sides) and −1, 1 (the unit circle), so the
-- sides of @g·F@ have endpoints @g(−½), g(½), g(∞), g(−1), g(1)@ — rational
-- numbers or ∞, exactly. A geodesic between two finite endpoints is the
-- semicircle with rational centre @(p + q)/2@ and rational radius
-- @|p − q|/2@; one with an endpoint at ∞ is a vertical line. So every side
-- is an exact SVG arc or line, and the only approximate quantities are the
-- two vertices @g(ρ)@, @g(ρ + 1)@, which "Flint.Ball" supplies. The Java
-- original had to sample each arc into up to 101 points because AWT can
-- only fill polygons; SVG can fill arcs.
module Modular.Geometry
  ( Geo(..), geodesic, Ideal(..), ideal
  , Seg(..), Shape(..), triangleShape
  , edgePoint, xExtent, visible, fitView
  ) where

import Data.Maybe (mapMaybe)
import Flint.Ball
import Flint.SL2
import Flint.Z
import Modular.Domain (Gen(..))

data Geo
  = Vert !Rational                                -- ^ the line Re z = x
  | Semi { gCenter :: !Rational, gRadius :: !Rational }
  deriving (Eq, Show)

geodesic :: QI -> QI -> Maybe Geo
geodesic Inf     (Fin x) = Just (Vert x)
geodesic (Fin x) Inf     = Just (Vert x)
geodesic (Fin x) (Fin y)
  | x == y    = Nothing
  | otherwise = Just (Semi ((x + y) / 2) (abs (x - y) / 2))
geodesic Inf Inf = Nothing

-- | The images of −½, ½, ∞, −1, 1.
data Ideal = Ideal { iL, iR, iInf, iA, iB :: QI } deriving (Show)

ideal :: SL2 -> Ideal
ideal g = Ideal (act g (Fin (-1 / 2))) (act g (Fin (1 / 2))) (act g Inf) (act g (Fin (-1))) (act g (Fin 1))

data Seg
  = ArcTo !Double !Bool !Screen   -- ^ radius in pixels, sweep flag, endpoint
  | LineTo !Screen
  deriving (Show)

data Shape = Shape
  { shStart :: !Screen
  , shSegs  :: [Seg]
  , shCusp  :: Maybe Screen   -- ^ where the triangle meets the real axis, if not at ∞
  } deriving (Show)

-- | Follow a geodesic from one screen point to another. On the upper
-- semicircle, moving to the right is clockwise on the screen, which is
-- SVG's positive sweep. A radius beyond 10⁷ px is drawn as a line: its
-- sagitta over any on-screen chord is under a twentieth of a pixel, and
-- browsers do not like arcs that large.
along :: View -> Maybe Geo -> Screen -> Screen -> Seg
along v (Just (Semi _ r)) from to
  | rpx <= 1.0e7 = ArcTo rpx (fst to > fst from) to
  where rpx = lengthPx v r
along _ _ _ to = LineTo to

triangleShape :: Prec -> View -> SL2 -> Shape
triangleShape p v g =
  case pInf of
    Inf ->
      -- g = [1 k; 0 1]: two vertical sides, cut off at the top of the view
      let xR = projectX v (maybe 0 id (finite pR))
          xL = projectX v (maybe 0 id (finite pL))
      in Shape vRho [bottom, LineTo (xR, -1), LineTo (xL, -1), LineTo vRho] Nothing
    Fin q ->
      let cp = (projectX v q, axisY v)
      in Shape vRho [bottom, along v (geodesic pR pInf) vRho1 cp, along v (geodesic pInf pL) cp vRho] (Just cp)
  where
    (vRho, vRho1) = case project p v g [Rho, Rho1] of
      [x, y] -> (x, y)
      _      -> error "project"
    Ideal pL pR pInf pA pB = ideal g
    bottom = along v (geodesic pA pB) vRho vRho1

-- | A point on the side named by a generator: where side-pairing markers go.
edgePoint :: Prec -> View -> SL2 -> Gen -> Screen
edgePoint p v g gen = case project p v g [base] of
  [x] -> x
  _   -> error "project"
  where
    base = case gen of
      T    -> EdgeR
      Tinv -> EdgeL
      S    -> PtI

-- | The real interval the triangle projects onto. At least four of the five
-- ideal points are finite, since a Möbius map sends only one point to ∞.
xExtent :: SL2 -> (Rational, Rational)
xExtent g = (minimum xs, maximum xs)
  where
    Ideal pL pR pInf pA pB = ideal g
    xs = mapMaybe finite [pL, pR, pInf, pA, pB]

-- | Every triangle touches the real axis, so horizontal culling is enough.
visible :: View -> SL2 -> Bool
visible v g = projectX v hi >= -margin && projectX v lo <= fromIntegral (vW v) + margin
  where
    (lo, hi) = xExtent g
    margin   = 4

-- | Centre and zoom that show all the given triangles.
fitView :: Int -> [SL2] -> (Rational, Rational)
fitView w gs = ((lo + hi) / 2, scale)
  where
    exts  = map xExtent gs
    lo    = minimum (map fst exts)
    hi    = maximum (map snd exts)
    width = max 1 (hi - lo)
    scale = (fromIntegral w * 9 / 10) / width
