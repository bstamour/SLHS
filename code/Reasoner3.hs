{-# LANGUAGE TypeFamilies #-}


module Reasoner where


import Control.Applicative
import qualified Data.Map as M


-- | A single-dimensional frame of discernment.
newtype Frame a = Frame a


-- | A multi-dimensional frame of discernment. The product of frames f and g 
--   (either of which can also be multi-dimensional.)
newtype PFrame f g = PFrame (f, g)




data MassIndex f = Items [f]       -- ^ A subset of the frame f.
                 | AllItems        -- ^ The whole frame.


newtype MassMap f = MassMap { unMassMap :: M.Map (MassIndex f) Rational }


newtype PMassMap m n = PMassMap (MassMapType m, MassMapType n)


-- Frames must have mass. For single dimensional frames, the mass is a simple
-- MassMap. For multi-dimensional frames, the mass is a tuple containing the
-- mass maps of the frames being multiplied.
type family MassMapType f
type instance MassMapType (Frame a)    = MassMap a
type instance MassMapType (PFrame f g) = PMassMap f g


-- | Assigns belief holders `h` to mass maps for the frame f. 
newtype MassAssignment h f = MassAssignment { unMA :: M.Map h (MassMapType f) }


-- | The main type: wraps the concept of running an expression through the
--   environment by threading a mass assignment into the statements.
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


{-
-- | Some operators are defined to work over multiple frames. Therefore
--   we need a way to run different types of environments with the same
--   interface.
class Runnable r where
  type MassType r
  run :: r a -> MassType r -> a


instance Runnable (Environment h f) where
  type MassType (Environment h f) = MassAssignment h f

  run = runEnvironment  
-}
