module Data.Geometry.Polygon.Morph.Linear where

import Control.Lens
import Data.Ext
import Data.Geometry.BezierSpline
import Data.Geometry.Point
import Data.Geometry.Polygon.Core (outerVertex, rotateRight, size, SimplePolygon)
import Data.List
import Data.Ord
import Control.Monad

-- | \( O(n^2) \)
alignToClosest :: (Num r, Ord r)
  => SimplePolygon p r -> SimplePolygon p r
  -> (SimplePolygon p r, SimplePolygon p r)
alignToClosest a b =
    (rotateRight aIdx a, rotateRight bIdx b)
  where
    pairs = liftM2 (,) [0 .. size a-1] [0 .. size b-1]
    (aIdx, bIdx) = minimumBy (comparing distance) pairs
    distance (l, r) = squaredEuclideanDist (a ^. outerVertex l.core) (b ^. outerVertex r.core)

addSteinerPoints :: ()
  => SimplePolygon p r -> SimplePolygon p r
  -> (SimplePolygon p r, SimplePolygon p r)
addSteinerPoints = undefined

addBezierSteinerPoints :: ()
  => SimplePolygon (LineJoin r) r -> SimplePolygon (LineJoin r) r
  -> (SimplePolygon (LineJoin r) r, SimplePolygon (LineJoin r) r)
addBezierSteinerPoints = undefined

linearInterpolate :: ()
  => SimplePolygon p r -> SimplePolygon p r
  -> r
  -> SimplePolygon p r
linearInterpolate = undefined

bezierInterpolate :: (Floating r)
  => SimplePolygon (LineJoin r) r -> SimplePolygon (LineJoin r) r
  -> r
  -> SimplePolygon (LineJoin r) r
bezierInterpolate = undefined
