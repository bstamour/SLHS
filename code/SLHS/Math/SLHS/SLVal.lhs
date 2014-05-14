



\ignore{
\begin{code}
module Math.SLHS.SLVal where

import Control.Applicative
import Control.Monad (ap)
\end{code}
}



\begin{code}
data SLVal a = SLVal a | Err String

err :: String -> SLVal a
err = Err

require :: Bool -> String -> SLVal ()
require True _ = pure ()
require False e = err e

instance Monad SLVal where
  return = SLVal
  SLVal x >>= f = f x
  Err e   >>= _ = Err e

instance Applicative SLVal where
  pure = return
  (<*>) = ap

instance Functor SLVal where
  fmap f ma = pure f <*> ma
\end{code}
