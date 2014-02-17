module Math.SL.Core where

import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import Math.SL.State

type SL a = SLState (Either String a)

runSL :: SL a -> SLStateObj -> SLValue a
runSL expr st = case evalState expr st of
  Left err -> SLError err
  Right x  -> SLValue x

-- | The result of an SL expression. Either it is a a value, or an error message.
data SLValue a = SLValue a | SLError String deriving (Show, Eq)

newtype Holder h = Holder h deriving (Show, Eq, Ord)

newtype Atom a = Atom a deriving (Show, Eq, Ord)
newtype Frame a = Frame { unFrame :: [Atom a] } deriving (Show, Eq, Ord)

newtype MassAssignment h a = MassAssignment { unMA :: M.Map (Holder h) (M.Map (Frame a) Rational) }

newtype BeliefVector a = BeliefVector { unBV :: M.Map (Frame a) Rational }
newtype BaseRateVector a = BaseRateVector { unBRV :: M.Map (Atom a) Rational }
