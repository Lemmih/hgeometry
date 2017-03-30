{-# LANGUAGE ScopedTypeVariables #-}
module Data.OrdSeqSpec where

import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Data.OrdSeq as OrdSeq
import           Data.OrdSeq (OrdSeq)
import           Data.Semigroup
import           Test.Hspec
import           Test.QuickCheck


instance (Arbitrary a, Ord a) => Arbitrary (OrdSeq a) where
  arbitrary = OrdSeq.fromListBy compare <$> arbitrary


spec :: Spec
spec = do
  describe "OrdSeq tests" $ do
    it "fromListBy" $
      property $ \(xs :: [Int]) ->
          F.toList (OrdSeq.fromListBy compare xs) `shouldBe` List.sort xs
    it "splitOn, <" $
      property $ \x (xs :: OrdSeq Int) ->
          let (l,_,_) = OrdSeq.splitOn id x xs
          in all (< x) l
    it "splitOn, ==" $
      property $ \x (xs :: OrdSeq Int) ->
          let (_,m,_) = OrdSeq.splitOn id x xs
          in all (== x) m
    it "splitOn, >=" $
      property $ \x (xs :: OrdSeq Int) ->
          let (_,_,r) = OrdSeq.splitOn id x xs
          in all (> x) r
    it "join" $
      property $ \(xs :: [Word]) -> let ys = map (+ maximum xs) xs in
          (F.toList $ OrdSeq.fromListBy compare xs <> OrdSeq.fromListBy compare ys)
          `shouldBe`
          List.sort (xs <> ys)
    it "positive member" $
      property $ \(xs :: OrdSeq Int) ->
         all (\x -> OrdSeq.memberBy compare x xs) xs
    it "member" $
      property $ \x (xs :: OrdSeq Int) ->
         OrdSeq.memberBy compare x xs
         `shouldBe`
         F.elem x (F.toList xs)
    it "lookupMin" $
       property $ \(xs :: OrdSeq Int) ->
         OrdSeq.lookupMin xs
         `shouldBe`
         (safe minimum $ F.toList xs)
    it "lookupMax" $
       property $ \(xs :: OrdSeq Int) ->
         OrdSeq.lookupMax xs
         `shouldBe`
         (safe maximum $ F.toList xs)


safe      :: ([t] -> a) -> [t] -> Maybe a
safe _ [] = Nothing
safe f xs = Just . f $ xs