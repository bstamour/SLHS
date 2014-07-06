
\documentclass[thesis.tex]{subfiles}

\begin{document}

\ignore{
\begin{code}
module Math.SLHS.Types where

import Data.Maybe
import Control.Applicative
import Control.Monad (ap)

import qualified Math.SLHS.Vector as V
import qualified Math.SLHS.Frame as F

import qualified Data.Map as M
\end{code}
}

\section{Core Types}

\subsection{Belief and Base Rate Vectors}

\begin{code}
type BeliefVector a = V.Vector a
type BaseRateVector a = V.Vector a
\end{code}

\subsection{Belief Holders}

\begin{code}
data Holder a = Holder a
              | Discount (Holder a) (Holder a)
              | Consensus (Holder a) (Holder a)
\end{code}

\subsection{Subjective Logic Values}

\begin{code}
data SLVal a = SLVal a | Err String

instance Monad SLVal where
  return = SLVal
  SLVal x >>= f = f x
  Err e   >>= _ = Err e

instance Applicative SLVal where
  pure = return
  (<*>) = ap

instance Functor SLVal where
  fmap = liftA
\end{code}

\begin{code}
err :: String -> SLVal a
err = Err
\end{code}

\begin{code}
require :: Bool -> String -> SLExpr h a (SLVal ())
require True _ = pure $ pure ()
require False e = pure $ err e
\end{code}

\subsection{Subjective Logic Expressions}

\begin{code}
data SLState h a = SLState { slsFrames     :: [F.Frame a]
                           , slsBeliefVecs :: M.Map h (BeliefVector a)
                           }
\end{code}

\begin{code}
data SLExpr h a t =
  SLExpr { runSLExpr :: SLState h a -> (SLState h a, SLVal t)
         }

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
  fmap = liftA
\end{code}



\end{document}
