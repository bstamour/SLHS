
module Reasoner where


import Control.Applicative
import qualified Data.Map as M


data MassIndex f = Items [f]    -- ^ A subset of the frame f.
                 | AllItems        -- ^ The whole frame.


newtype MassMap f = MassMap { unMassMap :: M.Map (MassIndex f) Rational }


newtype MassAssignment h f = MassAssignment { unMA :: M.Map h (MassMap f) }


newtype Environment h f a = Environment {
  runEnvironment :: MassAssignment h f -> a
}


instance Functor (Environment h f) where
  fmap f ex = Environment $ \mass -> f (runEnvironment ex mass)


instance Applicative (Environment h f) where
  pure x    = Environment $ \_ -> x

  ef <*> ex = Environment $ \mass -> let f = runEnvironment ef mass
                                         x = runEnvironment ex mass
                                     in f x
