module Math.SL.Core where

import Control.Monad
import Control.Applicative
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S


--------------------------------------------------------------------------------------------
-- Frames and their operations.
--------------------------------------------------------------------------------------------


newtype Frame a = Frame (S.Set a) deriving (Eq, Ord, Show)

frame :: Ord a => [a] -> Frame a
frame = Frame . S.fromList

frameToList :: Frame a -> [a]
frameToList (Frame s) = S.toList s

mapFrame :: (Ord a, Ord b) => (a -> b) -> Frame a -> [b]
mapFrame f (Frame s) = S.toList $ S.map f s


isSubframeOf :: Ord a => Frame a -> Frame a -> Bool
(Frame xs) `isSubframeOf` (Frame ys) = xs `S.isSubsetOf` ys

isElementOf :: Ord a => a -> Frame a -> Bool
x `isElementOf` (Frame xs) = x `S.member` xs

cardinality :: Frame a -> Int
cardinality (Frame s) = S.size s

difference :: Ord a => Frame a -> Frame a -> Frame a
difference (Frame xs) (Frame ys) = Frame $ S.difference xs ys


--------------------------------------------------------------------------------------------
-- Belief vectors and their operations.
--------------------------------------------------------------------------------------------


newtype BeliefVector a = BVec (M.Map (Frame a) Rational) deriving (Show, Eq)

beliefVector :: Ord a => Frame a -> [(Frame a, Rational)] -> SLValue (BeliefVector a)
beliefVector theta vals
  | lookup theta vals /= Nothing = SLError "error"
  | not (allInRange vals)        = SLError "error"
  | (total vals) > 1             = SLError "error"
  | otherwise                    = pure . BVec . M.fromList $ vals

beliefOf :: Ord a => BeliefVector a -> Frame a -> Rational
beliefOf (BVec m) theta = fromMaybe 0 (M.lookup theta m)



bvFocalElements :: BeliefVector a -> [Frame a]
bvFocalElements (BVec m) = M.keys m

bvUncertainty :: Ord a => BeliefVector a -> Rational
bvUncertainty (BVec m) = 1 - (total . M.toList $ m)

bvIsDefinedOver :: Ord a => BeliefVector a -> Frame a -> Bool
bvIsDefinedOver (BVec m) theta = all (`isSubframeOf` theta) . M.keys $ m


--------------------------------------------------------------------------------------------
-- Base rate vectors  and their operations.
--------------------------------------------------------------------------------------------


newtype BaseRateVector a = BRVec (M.Map a Rational) deriving (Show, Eq)

baseRateVector :: Ord a => Frame a -> [(a, Rational)] -> SLValue (BaseRateVector a)
baseRateVector _ vals | not (allInRange vals) = SLError "error"
                      | (total vals) > 1      = SLError "error"
                      | otherwise             = pure . BRVec . M.fromList $ vals

brvFocalElements :: BaseRateVector a -> [a]
brvFocalElements (BRVec m) = M.keys m

brvIsDefinedOver :: Ord a => BaseRateVector a -> Frame a -> Bool
brvIsDefinedOver (BRVec m) theta = all (`isElementOf` theta) . M.keys $ m


atomicityOf :: Ord a => BaseRateVector a -> a -> Rational
atomicityOf (BRVec m) x = fromMaybe 0 (M.lookup x m)



--------------------------------------------------------------------------------------------
-- SLValue: a monad for storing either values or error messages.
--------------------------------------------------------------------------------------------


data SLValue a = SLError String | SLValue a

instance Functor SLValue where
  fmap f x = pure f <*> x

instance Applicative SLValue where
  pure = return
  (<*>) = ap

instance Monad SLValue where
  return = SLValue
  (SLError s) >>= _ = SLError s
  (SLValue x) >>= f = f x


--------------------------------------------------------------------------------------------
-- Helper functions.
--------------------------------------------------------------------------------------------


inRange :: (Rational, Rational) -> Rational -> Bool
inRange (l, u) r = l <= r && r <= u

allInRange = all (inRange (0, 1)) . map snd
total = sum . map snd
