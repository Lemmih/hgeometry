{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.BezierSpline
  ( BezierSpline (BezierSpline)
  , evaluate
  , split
  , subBezier
  , tangent
  , approximate
  , parameterOf
  , snap
  ) where

import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector hiding (head, init, last)
import qualified Data.LSeq as LSeq
import           Data.LSeq (LSeq)
import           Data.Traversable (fmapDefault,foldMapDefault)

--------------------------------------------------------------------------------

-- newtype Bezier n d r = Bezier { controlPoints :: [Point d r] }

-- | Datatype representing a Bezier curve of degree n in d-dimensional space.
newtype BezierSpline d r = BezierSpline { controlPoints :: [Point d r] }

deriving instance (Arity d, Eq r) => Eq (BezierSpline d r)

type instance Dimension (BezierSpline d r) = d
type instance NumType   (BezierSpline d r) = r


instance (Arity d, Show r) => Show (BezierSpline d r) where
  show (BezierSpline ps) = mconcat [ "BezierSpline", show $ length ps - 1, " ", show ps ]

instance Arity d => Functor (BezierSpline d) where
  fmap = fmapDefault

instance Arity d => Foldable (BezierSpline d) where
  foldMap = foldMapDefault

instance Arity d => Traversable (BezierSpline d) where
  traverse f (BezierSpline ps) = BezierSpline <$> traverse (traverse f) ps





-- | Evaluate a BezierSpline curve at time t in [0, 1]
evaluate :: (Arity d, Ord r, Num r, Show r) => BezierSpline d r -> r -> Point d r
evaluate b t | t < 0 || t > 1                = error $ "Evaluation parameter " ++ show t ++ " out of bounds."
             | length (controlPoints b) == 0 = error "Bezier curve of degree -1?"
             | length (controlPoints b) == 1 = head $ controlPoints b
             | otherwise = flip evaluate t $ BezierSpline $ zipWith (blend t) (init $ controlPoints b) (tail $ controlPoints b)
  where blend t p q = p .+^ t *^ (q .-. p)

tangent :: (Arity d, Num r) => BezierSpline d r -> Vector d r
tangent b | length (controlPoints b) == 0 = error "Bezier curve of degree -1?"
          | length (controlPoints b) == 1 = error "Bezier curve of degree 0 has no tangent."
          | otherwise = controlPoints b !! 1 .-. controlPoints b !! 0

-- | Restrict a Bezier curve to the piece between parameters t < u in [0, 1].
subBezier     :: (Arity d, Ord r, Num r) => r -> r -> BezierSpline d r -> BezierSpline d r
subBezier t u = fst . split u . snd . split t

-- | Split a Bezier curve at time t in [0, 1] into two pieces.
split :: (Arity d, Ord r, Num r) => r -> BezierSpline d r -> (BezierSpline d r, BezierSpline d r)
split t b | t < 0 || t > 1                = error "Split parameter out of bounds."
          | length (controlPoints b) == 0 = error "Bezier curve of degree -1?"
          | otherwise = let n = length (controlPoints b) - 1 -- degree of curve
                            ps = collect t $ controlPoints b
                        in (BezierSpline $ take (n + 1) ps, BezierSpline $ drop n ps)

collect       :: (Arity d, Ord r, Num r) => r -> [Point d r] -> [Point d r]
collect _ []  = []
collect _ [p] = [p]
collect t ps  = [head ps] ++ collect t (zipWith (blend t) (init ps) (tail ps)) ++ [last ps]
  where blend t p q = p .+^ t *^ (q .-. p)

{-

-- | Merge to Bezier pieces. Assumes they can be merged into a single piece of the same degree
--   (as would e.g. be the case for the result of a 'split' operation).
--   Does not test whether this is the case!
merge :: (Arity d, Ord r, Num r) => (Bezier d r, Bezier d r) -> Bezier d r

-}

-- | Approximate Bezier curve by Polyline with given resolution.
--   TODO: Make collection of points more efficient! Currently quadratic.
approximate :: (Arity d, Ord r, Fractional r) => r -> BezierSpline d r -> [Point d r]
approximate r b | qdA (head $ controlPoints b) (last $ controlPoints b) < r ^ 2 = [head $ controlPoints b, last $ controlPoints b]
                | otherwise = let (b1, b2) = split 0.5 b
                              in approximate r b1 ++ tail (approximate r b2)

-- | Given a point on (or close to) a Bezier curve, return the corresponding parameter value.
--   (For points far away from the curve, the function will return the parameter value of
--   an approximate locally closest point to the input point.)
parameterOf :: (Arity d, Ord r, Fractional r, Show r) => BezierSpline d r -> Point d r -> r
parameterOf b p = binarySearch (qdA p . evaluate b) treshold (1 - treshold)
  where treshold = 0.0001

binarySearch :: (Ord r, Fractional r) => (r -> r) -> r -> r -> r
binarySearch f l r | abs (f l - f r) < treshold = m
                   | derivative f m  > 0 = binarySearch f l m
                   | otherwise           = binarySearch f m r
  where m = (l + r) / 2
        treshold = 0.0001

derivative     :: Fractional r => (r -> r) -> r -> r
derivative f x = (f (x + delta) - f x) / delta
  where delta = 0.00001

-- | Snap a point close to a Bezier curve to the curve.
snap   :: (Arity d, Ord r, Fractional r, Show r) => BezierSpline d r -> Point d r -> Point d r
snap b = evaluate b . parameterOf b
