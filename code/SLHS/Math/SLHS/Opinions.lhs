\documentclass[thesis.tex]{subfiles}

\begin{document}



\section{Opinions}

\ignore{
\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Math.SLHS.Opinions where

import Math.SLHS.Types
import qualified Data.Set as S
\end{code}
}


\subsection{Binomial Opinions}


We represent binomial opnions by four rational numbers corresponding to the belief,
disbelief, uncertainty, and base rate of the opinion, along with some additional
meta-data: the belief holder and the frame of discernment it is defined over. In code,
the binomial opinion looks like the following:


\begin{code}
data Binomial h a = Binomial { bBelief      :: Rational
                             , bDisbelief   :: Rational
                             , bUncertainty :: Rational
                             , bAtomicity   :: Rational
                             , bMetaData    :: MetaData h (BinaryFrame a)
                             }
\end{code}


and the meta-data is implemented as a \emph{generalized abstract data type}, which
allows for the constraining of the type parameter \emph{f} to consist of only those
types of the class \emph{FrameType}.


\begin{code}
data MetaData h f where
  MetaData :: FrameType f => Maybe (Holder h) -> f -> MetaData h f
\end{code}


We also introduce a special \emph{type class} called \emph{ToBinomial} which allows
us to define a range of types that can be converted to a binomial opinion. An example
of such a type could be a \emph{Beta PDF}.


\begin{code}
class ToBinomial op where
  toBinomial :: op h a -> Binomial h a

instance ToBinomial Binomial where
  toBinomial = id
\end{code}


\subsection{Multinomial Opinions}


\begin{code}
data Multinomial h a = Multinomial { mBelief      :: BeliefVector a
                                   , mUncertainty :: Rational
                                   , mBaseRate    :: BaseRateVector a
                                   , mMetaData    :: MetaData h (Frame a)
                                   }

class ToMultinomial op where
  toMultinomial :: op h a -> Multinomial h a
\end{code}


\subsection{Hyper Opinions}


\begin{code}
data Hyper h a = Hyper { hBelief      :: BeliefVector (Subframe a)
                       , hUncertainty :: Rational
                       , hBaseRate    :: BaseRateVector a
                       , hMetaData    :: MetaData h (Frame a)
                       }

class ToHyper op where
  toHyper :: op h a -> Hyper h a

instance ToHyper Hyper where
  toHyper = id
\end{code}








\subsection{The Opinion Type Class}


\begin{code}
class Opinion op h a where
  type FrameOf op h a :: * -> *
  type ExpectationType op h a :: *

  expectation :: op h a -> ExpectationType op h a
  frame       :: Ord a => op h a -> (FrameOf op h a) a


instance Opinion Binomial h a where
  type FrameOf Binomial h a = BinaryFrame
  type ExpectationType Binomial h a = Rational

  expectation (Binomial b d u a _) = b + a * u
  frame (Binomial _ _ _ _ (MetaData _ frm)) = frm


instance Opinion Multinomial h a where
  type FrameOf Multinomial h a = Frame
  type ExpectationType Multinomial h a = Vector a

  expectation _ = undefined
  frame _       = undefined


instance Opinion Hyper h a where
  type FrameOf Hyper h a = Frame
  type ExpectationType Hyper h a = Vector a

  expectation _ = undefined
  frame _       = undefined
\end{code}





\subsection{Belief Coarsening}



\subsubsection{Multinomial to Binomial Coarsening}



\begin{code}
coarsen :: ToMultinomial op => op h a -> Subframe a -> Binomial h a
coarsen = undefined
\end{code}





\subsubsection{Hypernomial to Multinomial Coarsening}


\begin{code}
hyperCoarsen :: ToHyper op => op h a -> [Subframe a] -> Multinomial h a
hyperCoarsen = undefined
\end{code}



\end{document}
