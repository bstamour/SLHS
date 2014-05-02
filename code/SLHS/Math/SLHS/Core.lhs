\documentclass[thesis.tex]{subfiles}

\begin{document}

\ignore{
\begin{code}
module Math.SLHS.Core
       ( SLValue(..)
       , SLState(..)
       , SLExpr()
       , runSL
       ) where

import Control.Monad
import Control.Applicative
\end{code}
}

\begin{code}
data SLValue a = Val a | Err String deriving (Eq, Show)

instance Monad SLValue where
  return = Val

  Val x >>= f = f x
  Err e >>= _ = Err e

instance Applicative SLValue where
  pure = return
  (<*>) = ap

instance Functor SLValue where
  fmap f x = pure f <*> x

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
\end{code}

\end{document}
