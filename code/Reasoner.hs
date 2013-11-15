{-# LANGUAGE TypeFamilies #-}


-----------------------------------------------------------------------------------
-- | Module for the core reasoning engine.
--
--   `Reasoner calcType atomType a` is a computation that represents a type
--   `a` computed through the reasoning system defined by the underlying reasoning
--   calculus `calcType` and `atomType`: a sum type representing the disjoint
--   atomic events (aka: a frame of discernment).
-----------------------------------------------------------------------------------


module Reasoner
       ( MassAssignment(..)
       , makeMassAssignment
       , Reasoner(..)
       , runReasoner
       , getMass
       ) where


import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Control.Applicative
import Control.Monad (ap)
import qualified Data.Map as M


-----------------------------------------------------------------------------------
-- | Core data types.
-----------------------------------------------------------------------------------


-- | Maps sets of atomType to belief mass assignments.
newtype MassAssignment atomType =
  MassAssignment
  { unMass :: M.Map [atomType] Rational
  }


-- | Create a mass assignment.
makeMassAssignment :: Ord atomType => [([atomType], Rational)]
                      -> MassAssignment atomType
makeMassAssignment = MassAssignment . M.fromList


-- | The reasoner object, parameterized by the underlying reasoning calculus,
--   the atoms that make up the frame of discernment, and the result of the
--   computation.
data Reasoner calcType atomType a =
  Reasoner
  { unR :: MassAssignment atomType -> a
  }


-- Define a functor between the category Hask and the subcategory
-- Reasoner calcType atomType.
instance Functor (Reasoner calcType atomType) where
  fmap f r = Reasoner $ \m -> f $ unR r m


-- There also exists a lax monoidal functor from Hask to Reasoner calcType atomType.
instance Applicative (Reasoner calcType atomType) where
  pure x    = Reasoner $ \_ -> x

  rf <*> rx = Reasoner $ \m -> let f = unR rf m
                               in  flip unR m $ fmap f rx


instance Monad (Reasoner calcType atomType) where
  return  = pure

  r >>= f = Reasoner $ \m -> let x = unR r m
                                 y = f x
                             in unR y m


-----------------------------------------------------------------------------------
-- | Helper functions.
-----------------------------------------------------------------------------------


-- | Run a computation with the given mass assignment.
runReasoner :: Reasoner calcType atomType a
               -> MassAssignment atomType
               -> a
runReasoner = unR


-- | Helper function: access the mass underneath the computation.
getMass :: Reasoner calcType atomType (MassAssignment atomType)
getMass = Reasoner $ \mass -> mass


-- | Get the mass of an event, or zero if the event is not in the mass assignment.
massOf :: Ord atomType
          => [atomType] -> Reasoner calcType atomType Rational
massOf event = fromMaybe 0 . M.lookup event . unMass <$> getMass


-----------------------------------------------------------------------------------
-- | Opinions defined across calculus types.
-----------------------------------------------------------------------------------


class Calculus calc where
  data Opinion calc
  (<>) :: Opinion calc -> Opinion calc -> Opinion calc


-----------------------------------------------------------------------------------
-- | Test code.
-----------------------------------------------------------------------------------


data Atoms = Red | Blue deriving (Show, Eq, Ord)
data SL = SL

instance Calculus SL where
  data Opinion SL = SLOpinion
  op1 <> op2 = undefined

type MyReasoner a = Reasoner SL Atoms a

mass = makeMassAssignment [ ([Red], 1%4)
                          , ([Blue], 1%4)
                          , ([Red, Blue], 1%2)
                          ]

--runTest :: Reasoner SL Atoms b -> b
runTest = flip runReasoner mass



runSL :: Reasoner SL atoms b -> MassAssignment atoms -> b
runSL = runReasoner

-- Some examples of applicative style.

-- | Get a list of all events in the mass assignment.
test0 = runTest $ map fst . M.toList . unMass <$> getMass

data FL = FL

-- Forced to execute in the SL monad.
test1 = flip runSL mass f
  where
    f :: Reasoner SL Atoms [[Atoms]] 
--    f :: Reasoner FL Atoms [[Atoms]] 
    f = map fst . M.toList . unMass <$> getMass

-- | Add the mass of [Red] to the mass of [Blue]
test2 = runTest $ (+) <$> massOf [Red] <*> massOf [Blue]


-- Some examples of monadic style.

-- | Find the masses of the items in the set using mapM from Control.Monad.
test3 = runTest $ mapM massOf [[Red], [Blue], [Red, Blue]]

-- | Same as test2 but with Monadic `do` notation.
test4 = runTest $ do massr <- massOf [Red]
                     massb <- massOf [Blue]
                     return $ massr + massb
