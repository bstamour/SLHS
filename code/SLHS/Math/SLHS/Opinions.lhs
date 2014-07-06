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
import qualified Math.SLHS.Vector as V
import qualified Math.SLHS.Frame as F

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
                             , bMetaData    :: MetaData h (F.BinaryFrame a)
                             }
\end{code}




\begin{code}
data MetaData h f = MetaData { mdHolder :: Maybe (Holder h)
                             , mdFrame  :: f
                             }
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
                                   , mMetaData    :: MetaData h (F.Frame a)
                                   }

class ToMultinomial op where
  toMultinomial :: op h a -> Multinomial h a

instance ToMultinomial Multinomial where
  toMultinomial = id
\end{code}


\subsection{Hyper Opinions}


\begin{code}
data Hyper h a = Hyper { hBelief      :: BeliefVector (F.Subframe a)
                       , hUncertainty :: Rational
                       , hBaseRate    :: BaseRateVector a
                       , hMetaData    :: MetaData h (F.Frame a)
                       }

class ToHyper op where
  toHyper :: op h a -> Hyper h a

instance ToHyper Hyper where
  toHyper = id
\end{code}








\subsection{The Opinion Type Class}


\begin{code}
class Opinion op h a where
  type FrameType op h a       :: *
  type ExpectationType op h a :: *

  expectation :: op h a -> ExpectationType op h a
  frame       :: op h a -> FrameType op h a

instance Opinion Binomial h a where
  type FrameType Binomial h a       = F.BinaryFrame a
  type ExpectationType Binomial h a = Rational

  expectation (Binomial b d u a _) = b + a * u
  frame (Binomial _ _ _ _ (MetaData _ frm)) = frm

instance Opinion Multinomial h a where
  type FrameType Multinomial h a       = F.Frame a
  type ExpectationType Multinomial h a = V.Vector a

  expectation _ = undefined
  frame (Multinomial _ _ _ (MetaData _ frm)) = frm

instance Opinion Hyper h a where
  type FrameType Hyper h a       = F.Frame a
  type ExpectationType Hyper h a = V.Vector a

  expectation _ = undefined
  frame (Hyper _ _ _ (MetaData _ frm)) = frm
\end{code}





\subsection{Belief Coarsening}



\subsubsection{Multinomial to Binomial Coarsening}



\begin{code}
coarsen :: (ToHyper op, Ord a) => op h a -> F.Subframe a -> Binomial h a
coarsen op theta = Binomial b d u a undefined
  where
    b = sum . map snd . V.elemsWhere (`F.isSubsetOf` theta) $ belief
    d = sum . map snd . V.elemsWhere (F.isEmpty . (`F.intersection` theta)) $ belief
    u = 1 - b - d
    a = sum . F.toList . F.map baseRate $ theta
    belief = hBelief . toHyper $ op
    baseRate x = V.value (hBaseRate . toHyper $ op) x
\end{code}





\subsubsection{Hypernomial to Multinomial Coarsening}


\begin{code}
hyperCoarsen :: ToHyper op => op h a -> [F.Subframe a] -> Multinomial h (F.Subframe a)
hyperCoarsen op thetas = Multinomial b u a undefined
  where
    b = undefined
    u = undefined
    a = undefined
\end{code}



\end{document}
