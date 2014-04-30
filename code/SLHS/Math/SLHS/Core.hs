

module Math.SLHS.Core
       ( SLValue(..)
       , SLState(..)
       , SLExpr()
       , runSL
       ) where

import Control.Monad
import Control.Applicative

data SLValue a = Val a | Err String

data SLState a = SLState a

newtype SLExpr s a = SLExpr { runSL :: s -> (SLValue a, s) }

instance Monad (SLExpr s) where
  return a = SLExpr $ \s -> (Val a, s)

  m >>= k = SLExpr $ \s -> case runSL m s of
    (Val x, s') -> runSL (k x) s'
    (Err e, s') -> (Err e, s')

instance Applicative (SLExpr s) where
  pure = return
  (<*>) = ap

instance Functor (SLExpr s) where
  fmap f x = pure f <*> x
