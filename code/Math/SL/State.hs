module Math.SL.State where

import Control.Monad
import Control.Applicative


data SLStateObj a = SLStateObj a


newtype SLState a t = SLState { unSLS :: [SLStateObj a] -> ([SLStateObj a], t) }

instance Functor (SLState a) where
  fmap f x = pure f <*> x

instance Applicative (SLState a) where
  pure = return
  (<*>) = ap

instance Monad (SLState a) where
  return x = SLState $ \st -> (st, x)
  sx >>= f = SLState $ \st -> let (st', x) = unSLS sx st
                                  sy       = f x
                                  y        = unSLS sy st'
                              in y

get :: SLState a [SLStateObj a]
get = SLState $ \st -> (st, st)
