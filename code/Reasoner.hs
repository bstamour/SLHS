module Reasoner
       ( MassAssignment(..)
       , makeMassAssignment
       , Reasoner(..)
       , runReasoner
       , getMass
       , massOf
       ) where


import Data.Maybe (fromMaybe)
import Control.Applicative
import qualified Data.Map as M


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


runReasoner :: Reasoner calcType atomType a
               -> MassAssignment atomType
               -> a
runReasoner = unR


getMass :: Reasoner calcType atomType (MassAssignment atomType)
getMass = Reasoner $ \mass -> mass


massOf :: Ord atomType
          => [atomType] -> Reasoner calcType atomType Rational
massOf event = fromMaybe 0 . M.lookup event . unMass <$> getMass

