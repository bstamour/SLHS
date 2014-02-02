{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Reasoner where

{- | This module represents the core reasoner architecture: reasoners, frames,
     belief mass assignments, and their respective instance declarations.

     For the time being, reasoners are defined only over one frame of discernment
     at a time, meaning certain SL operators (in particular, deduction and
     abduction) cannot work. I plan to solve this representational problem in
     the new year, but for now I want to focus on type-safe single frame reasoners.
-}

import Data.Ratio ((%))
import Data.Maybe (fromMaybe)
import Control.Applicative
import qualified Data.Map as M

data MassIndex f = Subset [f]    -- ^ Just a subset of the entire frame.
                 | Theta         -- ^ The whole thing.
                 deriving (Eq, Show)

instance (Ord f) => Ord (MassIndex f) where
  compare (Subset x) (Subset y) = compare x y
  compare Theta      Theta      = EQ
  compare (Subset _) Theta      = LT
  compare Theta      (Subset _) = GT

{- | Maps subsets of a frame f (or all of f) to fractions.

     Motivation:

     We use MassIndex types as the keys instead of the values directly
     so that users can freely use frames defined over massive types, such
     as Int, without having to store a key such as [minBound :: Int, maxBound :: Int]
     in the structure, which would cause massive slowdowns.
-}
newtype MassMap f = MassMap (M.Map (MassIndex f) Rational) deriving (Show)

-- | Associates belief holders (h) to mass maps.
newtype MassAssignment h f = MassAssignment (M.Map h (MassMap f)) deriving Show

data DMassIndex f = DElem f
                  | DTheta
                  deriving Eq

instance (Ord f) => Ord (DMassIndex f) where
  compare (DElem x) (DElem y) = compare x y
  compare DTheta    DTheta    = EQ
  compare (DElem _) DTheta    = LT
  compare DTheta    (DElem _) = GT

newtype DMassMap f = DMassMap (M.Map (DMassIndex f) Rational)

newtype DMassAssignment h f = DMassAssignment (M.Map h (DMassMap f))

{- The following heirarchy of BMA's exists so that we can constrain the main
   reasoner type over the kind of BMA that it carries around. BMA is the most
   general, and DirichletBMA is more restrictive, while still being a BMA.
-}

class BMA m h f where
  getHyperMass :: m h f -> MassAssignment h f

class BMA m h f => DirichletBMA m h f where
  getDirichletMass :: m h f -> DMassAssignment h f

instance BMA MassAssignment h f where
  getHyperMass = id

instance BMA DMassAssignment h f where
  getHyperMass (DMassAssignment m) = undefined

instance DirichletBMA DMassAssignment h f where
  getDirichletMass = id

newtype BaseRate f = BaseRate (M.Map f Rational) deriving Show

{- | The core reasoner type. Type-safe over the belief holders as well as 
     the frame of discernment. One cannot combine reasoners that are of 
     different types (for now.)

     Also parameterized over m, the mass type. Mass assignments can either 
     be BMA's, where anything goes; or Dirichlet BMA's, where mass must be 
     assigned only to the elements of the frame. The reason for this separation 
     is so that if we want to write expressions that are "ambiguous" and require 
     hyper opinions, we can use BMA. However if our observations are crisp and 
     we want to use multinomial opinions, then we can restrict ourselves to 
     dirichlet.

     To run a reasoner, you must supply it with a MassAssignment that is then 
     threaded through the equations, which are built up using applicative 
     combinators. All SL code must run within a reasoner to ensure type safety.
-}

newtype Reasoner m h f a = Reasoner { unR :: m h f -> BaseRate f -> a }

run :: BMA m h f => Reasoner m h f a -> m h f -> BaseRate f -> a
run = unR

instance Functor (Reasoner m h f) where
  fmap f rx = Reasoner $ \mass baseRate -> f (unR rx mass baseRate)

instance Applicative (Reasoner m h f) where
  pure x    = Reasoner $ \_ _ -> x
  rf <*> rx = Reasoner $ \mass baseRate -> let f = unR rf mass baseRate
                                               x = unR rx mass baseRate
                                           in f x

-- | Access the belief mass that we are towing around.
hyperMass :: (Ord h, Ord f, BMA m h f) => h -> Reasoner m h f (MassMap f)
hyperMass holder = Reasoner $ \m  _ ->
  let (MassAssignment m') = getHyperMass m
  in  fromMaybe emptyMass (M.lookup holder m')

dirichletMass :: (Ord h, Ord f, DirichletBMA m h f)
              => h
              -> Reasoner m h f (DMassMap f)
dirichletMass holder = Reasoner $ \ m _ ->
  let (DMassAssignment m') = getDirichletMass m
  in  fromMaybe emptyMass' (M.lookup holder m')

-- | In the absense of any mass, we assume total uncertainty.
emptyMass :: Ord f => MassMap f
emptyMass = MassMap $ M.fromList [(Theta, 1)]

emptyMass' :: Ord f => DMassMap f
emptyMass' = DMassMap $ M.fromList [(DTheta, 1)]

getBaseRate :: Reasoner m h f (BaseRate f)
getBaseRate = Reasoner $ flip const
