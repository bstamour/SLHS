{-# LANGUAGE TypeFamilies #-}


module Reasoner where


import Control.Applicative
import qualified Data.Map as M


data MassIndex f = Items [f]       -- ^ A subset of the frame f.
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


-- | Some operators are defined to work over multiple frames. Therefore
--   we need a way to run different types of environments with the same
--   interface.
class Runnable r where
  type MassType r
  run :: r a -> MassType r -> a


instance Runnable (Environment h f) where
  type MassType (Environment h f) = MassAssignment h f

  run = runEnvironment  
