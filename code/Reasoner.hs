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


-----------------------------------------------------------------------------------
-- The code Reasoner type.
-----------------------------------------------------------------------------------


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


getMass :: Reasoner calcType atomType (MassAssignment atomType)
getMass = Reasoner $ \mass -> mass


massOf :: Ord atomType
          => [atomType] -> Reasoner calcType atomType Rational
massOf event = fromMaybe 0 . M.lookup event . unMass <$> getMass


-----------------------------------------------------------------------------------
-- Allow for Reasoners to be composed easily.
--
-- The <~~ (thread) operator is for threading a mass assignment through the
-- equations. For example:
--
--     (opinion [Red, Blue] <&&> opinion [Blue, Green]) <~~ beliefMass
--
-- will compute the resulting opinion with respect to `beliefMass`. If you have
-- an operator that requires multiple frames of discernment, then you can feed
-- mass for each frame through via chaining, ala:
--
--     (opinion [Red] <@> opinion [Big]) <~~ colorMass <~~ sizeMass
--
-- (assuming the operator <@> has type
--
--     :: Reasoner SL a1 (Opinion SL)
--     -> Reasoner SL a2 (Opinion SL)
--     -> Combine (Reasoner SL a1) (Reasoner SL a2) (Opinion SL)
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


-----------------------------------------------------------------------------------
-- test shit.
-----------------------------------------------------------------------------------



--