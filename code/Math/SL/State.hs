module Math.SL.State where


import Control.Applicative
import Control.Monad


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
