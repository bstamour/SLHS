


module Core where


import Control.Monad
import Control.Applicative
import qualified Data.Map as M



data SLStateObj = SLStateObj
--               { slsBeliefMassAssignment :: Holder -> BMA
--               , slsBaseRate             :: Holder -> BaseRate
--               }


newtype SLState a = SLState { runState :: SLStateObj -> (SLStateObj, a) }

instance Monad SLState where
  return x = SLState $ \st -> (st, x)
  sa >>= f = SLState $ \st -> let (st', a)  = runState sa st
                                  sb        = f a
                                  (st'', b) = runState sb st'
                              in (st'', b)

instance Applicative SLState where
  pure = return
  (<*>) = ap

instance Functor SLState where
  fmap f sa = SLState $ \st -> let (st', a) = runState sa st
                             in  (st', f a)

evalState :: SLState a -> SLStateObj -> a
evalState st s = snd $ runState st s


----------------------------------------------------------------------------------


type SL a = SLState (Either String a)


runSL :: SL a -> SLStateObj -> SLValue a
runSL expr st = case evalState expr st of
  Left err -> SLError err
  Right x  -> SLValue x


-- | The result of an SL expression. Either it is a a value, or an error message.
data SLValue a = SLValue a | SLError String deriving (Show, Eq)


----------------------------------------------------------------------------------


-- Some of the core types.


newtype Holder h = Holder h deriving (Show, Eq, Ord)

newtype Atom a = Atom a deriving (Show, Eq, Ord)
newtype Frame a = Frame { unFrame :: [Atom a] } deriving (Show, Eq, Ord)


newtype MassAssignment h a = MassAssignment { unMA :: M.Map (Holder h) (M.Map (Frame a) Rational) }


newtype BeliefVector a = BeliefVector { unBV :: M.Map (Frame a) Rational }
newtype BaseRateVector a = BaseRateVector { unBRV :: M.Map (Atom a) Rational }
