module Foo where

import Data.Maybe
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

type Frame = S.Set
type Subframe = S.Set
type BeliefDistribution a = M.Map [a] Rational
type BaseRateVector a = M.Map a Rational

data BinomialOpinion a = BinOp
                         a
                         a
                         Rational
                         Rational
                         Rational
                         Rational

data Coarsened a = C (Subframe a) deriving (Eq, Ord)

data MultinomialOpinion a = MultOp
                            (Frame a)
                            (M.Map a Rational)
                            Rational
                            (M.Map a Rational)

data HyperOpinion a = HyperOp
                      (Frame a)
                      (M.Map (Subframe a) Rational)
                      Rational
                      (M.Map a Rational)

-----------------------------------------------------------------------------------------
-- Constructors.
-----------------------------------------------------------------------------------------

makeBinomial :: Frame a
             -> BeliefDistribution a
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

class Multinomial m where
  mBelief :: Ord a => m a -> a -> Maybe Rational
  mUncertainty :: m a -> Rational
  mBaseRate :: Ord a => m a -> a -> Maybe Rational

instance Multinomial BinomialOpinion where
  mBelief (BinOp x y b d _ _) e
    | e == x = Just b
    | e == y = Just d
    | otherwise = Nothing

  mUncertainty (BinOp _ _ _ _ u _) = u

  mBaseRate (BinOp x y _ _ _ a) e
    | e == x = Just a
    | e == y = Just (1 - a)
    | otherwise = Nothing

instance Multinomial MultinomialOpinion where
  mBelief (MultOp xs b _ _) x
    | x `S.member` xs = Just (fromMaybe 0 (M.lookup x b))
    | otherwise = Nothing

  mUncertainty (MultOp _ _ u _) = u

  mBaseRate (MultOp xs _ _ a) x
    | x `S.member` xs = Just (fromMaybe 0 (M.lookup x a))
    | otherwise = Nothing

class Hyper h where
  hBelief :: Ord a => h a -> Subframe a -> Maybe Rational
  hUncertainty :: h a -> Rational
  hBaseRate :: Ord a => h a -> a -> Maybe Rational

instance Hyper BinomialOpinion where
  hBelief (BinOp x y b d _ _) e
    | S.size e /= 1 = Nothing
    | e' == x = Just b
    | e' == y = Just d
    | otherwise = Nothing
    where
      [e'] = S.toList e

  hUncertainty (BinOp _ _ _ _ u _) = u

  hBaseRate (BinOp x y _ _ _ a) e
    | e == x = Just a
    | e == y = Just (1 - a)
    | otherwise = Nothing

instance Hyper MultinomialOpinion where
  hBelief (MultOp xs b _ _) e
    | S.size e /= 1 = Nothing
    | e' `S.member` xs = Just (fromMaybe 0 (M.lookup e' b))
    | otherwise = Nothing
    where
      [e'] = S.toList e

  hUncertainty (MultOp _ _ u _) = u

  hBaseRate (MultOp xs _ _ a) x
    | x `S.member` xs = Just (fromMaybe 0 (M.lookup x a))
    | otherwise = Nothing

instance Hyper HyperOpinion where
  hBelief (HyperOp xs b _ _) es
    | es `S.isSubsetOf` xs = Just (fromMaybe 0 (M.lookup es b))
    | otherwise = Nothing

  hUncertainty (HyperOp _ _ u _) = u

  hBaseRate (HyperOp xs _ _ a) x
    | x `S.member` xs = Just (fromMaybe 0 (M.lookup x a))
    | otherwise = Nothing

-----------------------------------------------------------------------------------------
-- Binomial operators.
-----------------------------------------------------------------------------------------

add :: Ord a
       => BinomialOpinion (Coarsened a)
       -> BinomialOpinion (Coarsened a)
       -> Maybe (BinomialOpinion (Coarsened a))
add (BinOp (C xs1) (C ys1) b1 d1 u1 a1) (BinOp (C xs2) (C ys2) b2 d2 u2 a2)
  | frm1 /= frm2 = Nothing
  | not (S.null $ xs1 `S.intersection` ys1) = Nothing
  | otherwise = Just $ BinOp (C newX) (C newY) b' d' u' a'
  where
    newX = xs1 `S.union` xs2
    newY = frm1 `S.difference` newX

    b' = b1 + b2
    d' = (a1 * (d1 - b2) + a2 * (d2 - b1)) / (a1 + a2)
    u' = (a1 * u1 + a2 * u2) / (a1 + a2)
    a' = a1 + a2

    frm1 = xs1 `S.union` ys1
    frm2 = xs2 `S.union` ys2

subtract :: Ord a
            => BinomialOpinion (Coarsened a)
            -> BinomialOpinion (Coarsened a)
            -> Maybe (BinomialOpinion (Coarsened a))
subtract (BinOp (C xs1) (C ys1) b1 d1 u1 a1) (BinOp (C xs2) (C ys2) b2 d2 u2 a2)
  | frm1 /= frm2 = Nothing
  | not (xs2 `S.isSubsetOf` xs1) = Nothing
  | otherwise = Just $ BinOp (C newX) (C newY) b' d' u' a'
  where
    newX = xs1 `S.union` xs2
    newY = frm1 `S.difference` newX

    b' = b1 - b2
    d' = (a1 * (d1 + b2) - a2 * (1 + b2 - b1 - u2)) / (a1 - a2)
    u' = (a1 * u1 - a2 * u2) / (a1 - a2)
    a' = a1 - a2

    frm1 = xs1 `S.union` ys1
    frm2 = xs2 `S.union` ys2

times :: Ord a
         => BinomialOpinion a
         -> BinomialOpinion a
         -> Maybe (BinomialOpinion a)
times op1 op2 = undefined

divide :: Ord a
          => BinomialOpinion a
          -> BinomialOpinion a
          -> Maybe (BinomialOpinion a)
divide = undefined

cotimes :: Ord a
           => BinomialOpinion a
           -> BinomialOpinion a
           -> Maybe (BinomialOpinion a)
cotimes = undefined

codivide :: Ord a
            => BinomialOpinion a
            -> BinomialOpinion a
            -> Maybe (BinomialOpinion a)
codivide = undefined

-----------------------------------------------------------------------------------------
-- Multinomial operators.
-----------------------------------------------------------------------------------------

multiply :: Multinomial op => op a -> op b -> HyperOpinion (a, b)
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
