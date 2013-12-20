


{- | This module represents the core reasoner architecture: reasoners, frames,
     belief mass assignments, and their respective instance declarations.

     For the time being, reasoners are defined only over one frame of discernment
     at a time, meaning certain SL operators (in particular, deduction and
     abduction) cannot work. I plan to solve this representational problem in
     the new year, but for now I want to focus on type-safe single frame reasoners.
-}


module Reasoner where


import Data.Maybe
import Control.Applicative
import qualified Data.Map as M


-- | A simple wrapper around frame types. For best effects, a should be bounded.
newtype Frame a = Frame a


data MassIndex f = Subset [f]    -- ^ Just a subset of the entire frame.
                 | Theta         -- ^ The whole thing.
                 deriving Eq


instance (Ord f) => Ord (MassIndex f) where
  (Subset x) < (Subset y) = x < y
  (Subset _) < Theta      = True
  Theta      < (Subset _) = False
  Theta      < Theta      = False


{- | Maps subsets of a frame f (or all of f) to fractions.

     Motivation:

     We use MassIndex types as the keys instead of the values directly
     so that users can freely use frames defined over massive types, such
     as Int, without having to store a key such as [minBound :: Int, maxBound :: Int]
     in the structure, which would cause massive slowdowns.
-}
newtype MassMap f = MassMap (M.Map (MassIndex f) Rational)


-- | Associates belief holders (h) to mass maps.
newtype MassAssignment h f = MassAssignment (M.Map h (MassMap f))


{- | The core reasoner type. Type-safe over the belief holders as well as the frame
     of discernment. One cannot combine reasoners that are of different types (for now.)

     To run a reasoner, you must supply it with a MassAssignment that is then threaded
     through the equations, which are built up using applicative combinators. All
     SL code must run within a reasoner to ensure type safety.
-}
newtype Reasoner h f a = Reasoner {
  unR :: MassAssignment h f -> a
}


instance Functor (Reasoner h f) where
  fmap f rx = Reasoner $ \mass -> f (unR rx mass)


instance Applicative (Reasoner h f) where
  pure x    = Reasoner $ \_ -> x

  rf <*> rx = Reasoner $ \mass -> let f = unR rf mass
                                      x = unR rx mass
                                  in f x


-- | Access the belief mass that we are towing around.
getMass :: (Ord h, Ord f) => h -> Reasoner h f (MassMap f)
getMass holder = Reasoner $ \ (MassAssignment m) -> fromMaybe emptyMass (M.lookup holder m)


-- | In the absense of any mass, we assume total uncertainty.
emptyMass :: Ord f => MassMap f
emptyMass = MassMap $ M.fromList [(Theta, 1)]