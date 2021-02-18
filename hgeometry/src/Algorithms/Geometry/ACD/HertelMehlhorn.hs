module Algorithms.Geometry.ACD.HertelMehlhorn where

import Control.Monad.ST
import Data.PlanarGraph.Immutable
import qualified Data.PlanarGraph.Mutable as Mut
import Data.Geometry.Point
import Data.Geometry.Polygon

-- | O(n)
acdPG :: (Num r, Ord r) => (Vertex -> Point 2 r) -> PlanarGraph -> PlanarGraph
acdPG vertexLocation pg = pgMutate pg gen
  where
    convexEdges = filter (isConvexEdge vertexLocation pg) (pgEdges pg)
    gen :: forall s. Mut.PlanarGraph s -> ST s ()
    gen mutPG = do
      let mutEdges = map (`Mut.edgeFromId` mutPG) $ map edgeId convexEdges
      mapM_ Mut.pgRemoveEdge mutEdges

isConvexEdge :: (Num r, Ord r) => (Vertex -> Point 2 r) -> PlanarGraph -> Edge -> Bool
isConvexEdge vertexLocation pg edge =
    halfEdgeIsInterior he1 pg &&
    halfEdgeIsInterior he2 pg &&
    ccw pTopR pTopM pTopL == CCW &&
    ccw pBotL pBotM pBotR == CCW
  where
    (he1, he2) = edgeHalfEdges edge
    pTopM = vertexLocation $ halfEdgeVertex he2 pg
    pTopL = vertexLocation $ halfEdgeTipVertex (halfEdgeNext he1 pg) pg
    pTopR = vertexLocation $ halfEdgeVertex (halfEdgePrev he2 pg) pg
    pBotM = vertexLocation $ halfEdgeVertex he1 pg
    pBotL = vertexLocation $ halfEdgeVertex (halfEdgePrev he1 pg) pg
    pBotR = vertexLocation $ halfEdgeTipVertex (halfEdgeNext he2 pg) pg

-- | O(n \log n)
acdPolygon :: Polygon t p r -> PlanarGraph
acdPolygon = undefined
