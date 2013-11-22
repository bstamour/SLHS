{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}


module Reasoner
       ( MassAssignment(..)
       , makeMassAssignment
       , Reasoner(..)
 --      , runReasoner
       , getMass
       , massOf
       , run
       , (<~~)
       ) where


import Data.Maybe (fromMaybe)
import Control.Applicative
import qualified Data.Map as M

import Data.Functor.Compose


newtype MassAssignment atomType =
  MassAssignment
  { unMass :: M.Map [atomType] Rational
  }


makeMassAssignment :: Ord atomType => [([atomType], Rational)]
                      -> MassAssignment atomType
makeMassAssignment = MassAssignment . M.fromList


data Reasoner calcType atomType a =
  Reasoner
  { unR :: MassAssignment atomType -> a
  }


instance Functor (Reasoner calcType atomType) where
  fmap f r = Reasoner $ \m -> f $ unR r m


instance Applicative (Reasoner calcType atomType) where
  pure x    = Reasoner $ \_ -> x

  rf <*> rx = Reasoner $ \m -> let f = unR rf m
                               in  flip unR m $ fmap f rx


instance Monad (Reasoner calcType atomType) where
  return  = pure

  r >>= f = Reasoner $ \m -> let x = unR r m
                                 y = f x
                             in unR y m


{-
runReasoner :: Reasoner calcType atomType a
               -> MassAssignment atomType
               -> a
runReasoner = unR
-}


getMass :: Reasoner calcType atomType (MassAssignment atomType)
getMass = Reasoner $ \mass -> mass


massOf :: Ord atomType
          => [atomType] -> Reasoner calcType atomType Rational
massOf event = fromMaybe 0 . M.lookup event . unMass <$> getMass


-----------------------------------------------------------------------------------
-- Allow for Reasoners to be composed easily.
-----------------------------------------------------------------------------------


class RunReasoner r where
  type MassType r
  type ResultType r
  run :: r -> MassType r -> ResultType r


instance RunReasoner (Reasoner c a t) where
  type MassType   (Reasoner c a t) = MassAssignment a
  type ResultType (Reasoner c a t) = t
  run = unR


instance (RunReasoner (r2 t)) => RunReasoner (Compose (Reasoner c1 a1) r2 t) where
  type MassType   (Compose (Reasoner c1 a1) r2 t) = MassAssignment a1
  type ResultType (Compose (Reasoner c1 a1) r2 t) = r2 t
  run = unR . getCompose


(<~~) :: RunReasoner r => r -> MassType r -> ResultType r
(<~~) = run 







