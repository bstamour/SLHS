

\ignore{
\begin{code}
module Math.SLHS.Types where

import Control.Applicative
import Control.Monad (ap)
import qualified Data.Set as S
import qualified Data.Map as M
\end{code}
}




\begin{code}
type Frame a = S.Set a
type Subframe a = Frame a
\end{code}


\begin{code}
newtype Holder a = Holder a
\end{code}



\begin{code}
data BeliefVector a = BV a
\end{code}


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


\begin{code}
data SLState h a =
  SLState { slsFrames     :: [Frame a]
          , slsBeliefVecs :: M.Map h (BeliefVector a)
          }
\end{code}



\begin{code}
data SLExpr h a t = SLExpr { runSLExpr :: SLState h a -> (SLState h a, SLVal t) }

instance Monad (SLExpr h a) where
  return x = SLExpr $ \st -> (st, SLVal x)
  ma >>= f = SLExpr $ \st -> let (st', a) = runSLExpr ma st
                             in case a of
                               Err e   -> (st', Err e)
                               SLVal x -> runSLExpr (f x) st'

instance Applicative (SLExpr h a) where
  pure = return
  (<*>) = ap

instance Functor (SLExpr h a) where
  fmap f ma = pure f <*> ma
\end{code}
