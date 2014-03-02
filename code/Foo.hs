module Foo where


import Data.Maybe
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S


type Frame = S.Set
type Subframe = S.Set
type BeliefDistribution a = M.Map [a] Rational
type BaseRateVector a = M.Map a Rational


data BinomialOpinion a = BinomialOpinion
                         a
                         a
                         Rational
                         Rational
                         Rational
                         Rational


data Coarsened a = X (Subframe a)
                 | NotX (Subframe a)
                 deriving (Eq, Ord)


data MultinomialOpinion a = MultinomialOpinion
                            (Frame a)
                            (M.Map a Rational)
                            Rational
                            (M.Map a Rational)


data HyperOpinion a = HyperOpinion
                      (Frame a)
                      (M.Map (Subframe a) Rational)
                      Rational
                      (M.Map a Rational)


-----------------------------------------------------------------------------------------
-- Constructors.
-----------------------------------------------------------------------------------------


makeBinomial :: BeliefDistribution a
             -> BaseRateVector a
             -> Maybe (BinomialOpinion a)
makeBinomial = undefined


makeMultinomial :: BeliefDistribution a
                -> BaseRateVector a
                -> Maybe (MultinomialOpinion a)
makeMultinomial = undefined


makeHyper :: BeliefDistribution a
          -> BaseRateVector a
          -> Maybe (HyperOpinion a)
makeHyper = undefined


-----------------------------------------------------------------------------------------
-- Accessor functions.
-----------------------------------------------------------------------------------------


bBelief :: BinomialOpinion a -> Rational
bBelief (BinomialOpinion _ _ b _ _ _) = b


bDisbelief :: BinomialOpinion a -> Rational
bDisbelief (BinomialOpinion _ _ _ d _ _) = d


bUncertainty :: BinomialOpinion a -> Rational
bUncertainty (BinomialOpinion _ _ _ _ u _) = u


bBaseRate :: BinomialOpinion a -> Rational
bBaseRate (BinomialOpinion _ _ _ _ _ a) = a


class Multinomial m where
  mBelief      :: Ord a => m a -> a -> Maybe Rational
  mUncertainty :: m a -> Rational
  mBaseRate    :: Ord a => m a -> a -> Maybe Rational


instance Multinomial BinomialOpinion where
  mBelief (BinomialOpinion x y b d _ _) e
    | e == x    = Just b
    | e == y    = Just d
    | otherwise = Nothing

  mUncertainty (BinomialOpinion _ _ _ _ u _) = u

  mBaseRate (BinomialOpinion x y _ _ _ a) e
    | e == x    = Just a
    | e == y    = Just (1 - a)
    | otherwise = Nothing


instance Multinomial MultinomialOpinion where
  mBelief (MultinomialOpinion xs b _ _) x
    | x `S.member` xs = Just (fromMaybe 0 (M.lookup x b))
    | otherwise       = Nothing

  mUncertainty (MultinomialOpinion _ _ u _) = u

  mBaseRate (MultinomialOpinion xs _ _ a) x
    | x `S.member` xs = Just (fromMaybe 0 (M.lookup x a))
    | otherwise       = Nothing


class Hyper h where
  hBelief      :: Ord a => h a -> Subframe a -> Maybe Rational
  hUncertainty :: h a -> Rational
  hBaseRate    :: Ord a => h a -> a -> Maybe Rational


instance Hyper BinomialOpinion where
  hBelief (BinomialOpinion x y b d _ _) e
    | S.size e /= 1 = Nothing
    | e' == x       = Just b
    | e' == y       = Just d
    | otherwise     = Nothing
    where
      [e'] = S.toList e

  hUncertainty (BinomialOpinion _ _ _ _ u _) = u

  hBaseRate (BinomialOpinion x y _ _ _ a) e
    | e == x    = Just a
    | e == y    = Just (1 - a)
    | otherwise = Nothing


instance Hyper MultinomialOpinion where
  hBelief (MultinomialOpinion xs b _ _) e
    | S.size e /= 1    = Nothing
    | e' `S.member` xs = Just (fromMaybe 0 (M.lookup e' b))
    | otherwise        = Nothing
    where
      [e'] = S.toList e

  hUncertainty (MultinomialOpinion _ _ u _) = u

  hBaseRate (MultinomialOpinion xs _ _ a) x
    | x `S.member` xs = Just (fromMaybe 0 (M.lookup x a))
    | otherwise       = Nothing


instance Hyper HyperOpinion where
  hBelief (HyperOpinion xs b _ _) es
    | es `S.isSubsetOf` xs = Just (fromMaybe 0 (M.lookup es b))
    | otherwise            = Nothing

  hUncertainty (HyperOpinion _ _ u _) = u

  hBaseRate (HyperOpinion xs _ _ a) x
    | x `S.member` xs = Just (fromMaybe 0 (M.lookup x a))
    | otherwise       = Nothing


-----------------------------------------------------------------------------------------
-- Binomial operators.
-----------------------------------------------------------------------------------------


add :: BinomialOpinion (Coarsened a)
    -> BinomialOpinion (Coarsened a)
    -> Maybe (BinomialOpinion (Coarsened a))
add = undefined


subtract :: BinomialOpinion (Coarsened a)
         -> BinomialOpinion (Coarsened a)
         -> Maybe (BinomialOpinion (Coarsened a))
subtract = undefined


times :: BinomialOpinion (Coarsened a)
      -> BinomialOpinion (Coarsened a)
      -> Maybe (BinomialOpinion (Coarsened a))
times = undefined


divide :: BinomialOpinion (Coarsened a)
       -> BinomialOpinion (Coarsened a)
       -> Maybe (BinomialOpinion (Coarsened a))
divide = undefined


cotimes :: BinomialOpinion (Coarsened a)
        -> BinomialOpinion (Coarsened a)
        -> Maybe (BinomialOpinion (Coarsened a))
cotimes = undefined


codivide :: BinomialOpinion (Coarsened a)
         -> BinomialOpinion (Coarsened a)
         -> Maybe (BinomialOpinion (Coarsened a))
codivide = undefined


-----------------------------------------------------------------------------------------
-- Multinomial operators.
-----------------------------------------------------------------------------------------


multiply :: Multinomial op => op a -> op a -> HyperOpinion a
multiply = undefined


-----------------------------------------------------------------------------------------
-- Hyper operators.
-----------------------------------------------------------------------------------------


cfuse :: Hyper op => op a -> op a -> HyperOpinion a
cfuse = undefined


afuse :: Hyper op => op a -> op a -> HyperOpinion a
afuse = undefined


-----------------------------------------------------------------------------------------
-- State.
-----------------------------------------------------------------------------------------


data SLData h a = SLData
                  { sldFrames :: [Frame a]
                  , sldOwners :: [h]
                  }


type SLState h a t = State (SLData h a)
