

\ignore{
\begin{code}
module Math.SLHS.SLExpr where

import Math.SLHS.SLVal
import Math.SLHS.BeliefVector
import Math.SLHS.Frame
import Control.Applicative
import Control.Monad (ap)
import qualified Data.Map as M
\end{code}
}




\begin{code}
data SLState h a = SLState { slsFrames     :: [Frame a]
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
