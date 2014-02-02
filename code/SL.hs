module SL where

{- | SL-related code.

     Below we define hyper opinions first, as they're the most general form
     of subjective opinion. Afterward we define multinomial opinions and methods to 
     (possibly) generate a multinomial from a hyper. Finally the last, and most 
     interesting class, is the binomial opinion.
-}

import Reasoner
import Data.Maybe (fromMaybe)
import Control.Applicative
import qualified Data.Map as M

-- | The most general form of opinion in Subjective Logic.
data HyperOpinion f =
  HyperOpinion
  { hyperBelief      :: M.Map [f] Rational
  , hyperUncertainty :: Rational
  , hyperBaseRate    :: M.Map f Rational
  } deriving Show

-- | Construct a hyper opinion from the belief mass.
opinion :: (Ord h, Ord f, BMA m h f) => h -> Reasoner m h f (HyperOpinion f)
opinion holder = go <$> hyperMass holder <*> getBaseRate
  where
    go (MassMap m) (BaseRate br) = let u  = fromMaybe 0 (M.lookup Theta m)
                                       m' = M.delete Theta m
                                       b  = M.mapKeys (\(Subset s) -> s) m'
                                       a  = br
                                   in HyperOpinion b u a

-- | Not as general as a hyper opinion.
data MultinomialOpinion f =
  MultinomialOpinion
  { multiBelief      :: M.Map f Rational
  , multiUncertainty :: Rational
  , multiBaseRate    :: M.Map f Rational
  } deriving Show

-- | Multinomial opinions can only be constructed using Dirichlet BMA's.
multinomial :: (DirichletBMA m h f) 
            => h 
            -> Reasoner m h f (MultinomialOpinion f)
multinomial = undefined

-- | A binomial opinion is either a binary frame, or a binary partitioning
--   of a multinomial frame.
data BinomialOpinion f =
  BinomialOpinion
  { binBelief      :: Rational
  , binDisbelief   :: Rational
  , binUncertainty :: Rational
  , binBaseRate    :: Rational
  } deriving Show

-- | Given a subset x of f, compute the binomial opinion over the binary partition
--   frame {x, ~x}.
binomial :: BMA m h f => h -> [f] -> Reasoner m h f (BinomialOpinion f)
binomial = undefined

{- Subjective Logic operators -}

-- These instances just make the equations easier to write, instead of
-- lifting all of the mathematical operators into the applicative.

instance Num a => Num (Reasoner m h f a) where
  rx + ry       = (+)    <$> rx <*> ry
  rx * ry       = (*)    <$> rx <*> ry
  rx - ry       = (-)    <$> rx <*> ry
  negate rx     = negate <$> rx
  abs rx        = abs    <$> rx
  signum rx     = signum <$> rx
  fromInteger i = pure (fromInteger i)

instance Fractional a => Fractional (Reasoner m h f a) where
  rx / ry        = (/) <$> rx <*> ry
  fromRational r = pure (fromRational r)

binomialSum :: Reasoner m h f (BinomialOpinion f)
            -> Reasoner m h f (BinomialOpinion f)
            -> Reasoner m h f (BinomialOpinion f)
binomialSum op1 op2 =
  let b = bx + by
      d = (ax * (dx - by) + ay * (dy - bx)) / (ax + ay)
      u = (ax * ux + ay * uy) / (ax + ay)
      a = ax + ay
  in BinomialOpinion <$> b <*> d <*> u <*> a
  where
    bx = binBelief      <$> op1
    by = binBelief      <$> op2
    dx = binDisbelief   <$> op1
    dy = binDisbelief   <$> op2
    ux = binUncertainty <$> op1
    uy = binUncertainty <$> op2
    ax = binBaseRate    <$> op1
    ay = binBaseRate    <$> op2

binomialDiff :: Reasoner m h f (BinomialOpinion f)
             -> Reasoner m h f (BinomialOpinion f)
             -> Reasoner m h f (BinomialOpinion f)
binomialDiff op1 op2 =
  let b = bx - by
      d = (ax * (dx + by) - ay * (1 + by - bx - uy)) / (ax - ay)
      u = (ax * ux - ay * uy) / (ax - ay)
      a = ax - ay
  in BinomialOpinion <$> b <*> d <*> u <*> a
  where
    bx = binBelief      <$> op1
    by = binBelief      <$> op2
    dx = binDisbelief   <$> op1
    dy = binDisbelief   <$> op2
    ux = binUncertainty <$> op1
    uy = binUncertainty <$> op2
    ax = binBaseRate    <$> op1
    ay = binBaseRate    <$> op2
