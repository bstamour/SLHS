module Math.SL.State where


import Math.SL.Core
import Control.Applicative
import Control.Monad


data SLStateObj h a = SLStateObj
                      { slsBeliefMassAssignment :: MassAssignment h a
                      , slsBaseRate             :: BaseRateAssignment h a
                      }


newtype SLState h a t = SLState { runState :: SLStateObj h a -> (SLStateObj h a, t) }

instance Monad (SLState h a) where
  return x = SLState $ \st -> (st, x)

  sa >>= f = SLState $ \st -> let (st', a) = runState sa st
                                  sb       = f a
                              in runState sb st'

instance Applicative (SLState h a) where
  pure  = return
  (<*>) = ap

instance Functor (SLState h a) where
  fmap f sa = pure f <*> sa


runSL :: SLState h a t -> SLStateObj h a -> t
runSL st s = snd $ runState st s





type SL h a t = SLState h a (SLValue t)
