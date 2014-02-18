module Math.SL.Core where


import Control.Monad
import Control.Applicative
import qualified Data.Map as M


-- | The result of an SL expression. Either it is a a value, or an error message.
data SLValue a = SLValue a | SLError String deriving (Show, Eq)

instance Functor SLValue where
  fmap f (SLValue x) = SLValue (f x)
  fmap _ (SLError e) = SLError e

instance Applicative SLValue where
  pure  = return
  (<*>) = ap

instance Monad SLValue where
  return x            = SLValue x
  (SLValue x)   >>= f = f x
  (SLError err) >>= _ = SLError err


-- | A belief holder.
newtype Holder h = Holder h deriving (Show, Eq, Ord)


-- | An atomic event.
newtype Atom a = Atom a deriving (Show, Eq, Ord)


-- | A frame of mutually atomic events.
data Frame a = Frame [Atom a]
             | ProductFrame (Frame a) (Frame a)
             deriving (Show, Eq, Ord)


-- | A mass assignment. For each holder, assign a basic belief assignment.
newtype MassAssignment h a =
  MassAssignment { unMA :: M.Map (Holder h) (M.Map (Frame a) Rational) }


-- | A base rate assignment.
newtype BaseRateAssignment h a =
  BaseRateAssignment { unBRA :: M.Map (Holder h) (M.Map (Atom a) Rational) }


-- | Beliefs are maps from frames to numbers.
newtype BeliefVector a = BeliefVector { unBV :: M.Map (Frame a) Rational }


-- | Base rates are maps from atoms to numbers.
newtype BaseRateVector a = BaseRateVector { unBRV :: M.Map (Atom a) Rational }
