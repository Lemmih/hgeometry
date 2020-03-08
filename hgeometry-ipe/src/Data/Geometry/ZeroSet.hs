{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.ZeroSet where

import           Control.Lens
import           Data.Bitraversable
import           Data.Bifoldable
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           GHC.Generics

import           Debug.Trace

--------------------------------------------------------------------------------

-- | side lengths will be 2^i for some integer i
type SI = Int


-- data QuadTree v p r = QuadTree { _startLowerLeft :: !(Point 2 r)
--                                , _tree           :: QuadTree' v p
--                                } deriving (Show,Eq)



data Quadrants a = Quadrants { _northEast  :: !a
                             , _southEast  :: !a
                             , _southWest  :: !a
                             , _northWest  :: !a
                             } deriving (Show,Eq,Ord,Generic,Functor,Foldable,Traversable)
makeLenses ''Quadrants

data Cell r = Cell { _lowerLeft  :: !(Point 2 r)
                   , _sideLenghI :: !SI
                   } deriving (Show,Eq,Generic,Functor,Foldable,Traversable)

makeLenses ''Cell

data QuadTree' q v p = QTLeaf p | QTNode { _nodeData   :: !v
                                         , _quadrants  :: !(Quadrants q)
                                         } deriving (Show,Eq,Generic)
makePrisms ''QuadTree'

instance Bifunctor (QuadTree' q) where
  bimap = bimapDefault
instance Bifoldable (QuadTree' q) where
  bifoldMap = bifoldMapDefault
instance Bitraversable (QuadTree' q) where
  bitraverse f g = \case
    QTLeaf p    -> QTLeaf <$> g p
    QTNode v qs -> flip QTNode qs <$> f v

data QuadTree v p r = QuadTree { _cell :: !(Cell r)
                               , _tree :: !(QuadTree' (QuadTree v p r) v p)
                               } deriving (Show,Eq)
makeLenses ''QuadTree

pattern Leaf     :: Cell r -> p -> QuadTree v p r
pattern Leaf c p = QuadTree c (QTLeaf p)

pattern Node        :: Cell r -> v -> Quadrants (QuadTree v p r) -> QuadTree v p r
pattern Node c v qs = QuadTree c (QTNode v qs)

{-# COMPLETE Leaf, Node #-}

instance Functor (QuadTree v p) where
  fmap f = trimap id id f


trimap       :: (v -> v') -> (p -> p') -> (r -> r') -> QuadTree v p r -> QuadTree v' p' r'
trimap f g h = runIdentity . tritraverse (pure . f) (pure . g) (pure . h)

tritraverse       :: Applicative f => (v -> f v') -> (p -> f p') -> (r -> f r')
                  -> QuadTree v p r -> f (QuadTree v' p' r')
tritraverse f g h = go
  where
    go (QuadTree c t) = QuadTree <$> traverse h c <*> goT t

    goT = \case
      QTLeaf p    -> QTLeaf <$> g p
      QTNode v qs -> QTNode <$> f v <*> traverse go qs


-- | Gets all cells
cells :: QuadTree v p r -> NonEmpty (Cell r :+ Either p v)
cells = \case
          Leaf c p    -> (c :+ Left p)  :| []
          Node c v qs -> (c :+ Right v) :| concatMap (NonEmpty.toList . cells) qs


-- buildQuadTree'          :: Point 2 Int -> SI -> Split v p
--                         -> (Split v p -> Split v p) -> QuadTree' v p
-- buildQuadTree' o w s = case s of
--                             No p  -> QTLeaf p
--                             Yes v -> QTNode o w v (buildQuadTree' (o .+^ Vector2 r r) r )
--   where
--     r = w `div` 2




data Sign = Negative | Zero | Positive deriving (Show,Eq,Ord)

testSign   :: (Ord r, Num r) => (p -> r) -> p -> Sign
testSign f = \p -> case f p `compare` 0 of
                     LT -> Negative
                     EQ -> Zero
                     GT -> Positive




offset      :: Num r => Vector 2 r -> QuadTree v p r -> QuadTree v p r
offset v qt = qt&cell.lowerLeft %~ (.+^ v)

withOffset     :: Num r => Vector 2 r -> (Point 2 r -> a) -> (Point 2 r -> a)
withOffset v f = \p -> f $ p .+^ v

data Split p = No !p | Yes deriving (Show,Read,Eq,Ord)
makePrisms ''Split

-- | Given a function with which to label the corners, and a test if
-- we should keep splitting builds a quadtree on the square \([0,2^i]
-- \times [0,2^i]\).
buildQT                 :: forall r p. Fractional r
                        => (Point 2 r -> p)
                        -> (SI -> Quadrants p -> Split p)
                        -> SI
                        -> QuadTree (Quadrants p) p r
buildQT f shouldSplit i = QuadTree (Cell origin i) $ case shouldSplit i corners of
                                                       No p -> QTLeaf p
                                                       Yes  -> QTNode corners chs
  where
    corners = f <$> Quadrants (Point2 w w) (Point2 w 0) origin (Point2 0 w)
    w = 2 `pow` i
    j = i - 1
    r = 2 `pow` j

    chs = build <$> Quadrants (Vector2 r r) (Vector2 r 0) (Vector2 0 0) (Vector2 0 r)

    build   :: Vector 2 r -> QuadTree (Quadrants p) p r
    build v = offset v $ buildQT (withOffset v f) shouldSplit j

shouldSplit'                                   :: Eq p => p -> SI -> Quadrants p -> Split p
shouldSplit' z i corners | all (sgn ==) corners = No sgn
                         | i < 0                = No z
                         | otherwise            = Yes
  where
    sgn = _northEast corners

pow                 :: Fractional r => r -> Int -> r
pow b i | i < 0     = 1 / (b ^ (abs i))
        | otherwise = b ^ i












test = buildQT (testSign $ \p -> p^.yCoord - (0.5 * p^.xCoord + 0.1)) (shouldSplit' Zero) 2