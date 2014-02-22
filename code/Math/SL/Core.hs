module Math.SL.Core where


import Math.SL.Frame
import Math.SL.SLValue

import Control.Monad
import Control.Applicative

import qualified Data.Map as M


-- | A belief holder.
newtype Holder h = Holder h deriving (Show, Eq, Ord)


-- | An atomic event.
newtype Atom a = Atom a deriving (Show, Eq, Ord)


-- TODO: Look these over.

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
