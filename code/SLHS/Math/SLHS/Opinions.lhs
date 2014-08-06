\documentclass[thesis.tex]{subfiles}

\begin{document}



\section{Opinions}

In this section we will discuss the implementation of the most important
objects in SLHS from the user's standpoint: Subjective opinions. We start by
implementing binomial opinions, and then we present multinomial and hyper
opinions.


\ignore{
\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Math.SLHS.Opinions where

import Math.SLHS.Types
import qualified Math.SLHS.Vector as V
import qualified Math.SLHS.Frame as F

import qualified Data.Set as S
\end{code}
}


\subsection{Binomial Opinions}

We represent binomial opnions by four rational numbers corresponding
to the belief, disbelief, uncertainty, and base rate of the opinion,
along with some additional meta-data: the belief holder and the frame
of discernment it is defined over. In code, the binomial opinion looks
like the following:

\begin{code}
data Binomial h a = Binomial { bBelief      :: Rational
                             , bDisbelief   :: Rational
                             , bUncertainty :: Rational
                             , bAtomicity   :: Rational
                             , bMetaData    :: MetaData h (F.BinaryFrame a)
                             }
\end{code}

Here we use Haskell's \emph{record syntax} to define the data constructor.
Haskell automatically creates the top-level functions \emph{bBelief},
\emph{bDisbelief}, \emph{bUncertainty}, \emph{bAtomicity}, and
\emph{bMetaData} that provide access to the "members" of the record.

The meta-data associated with the opinion is also defined using the record
syntax:

\begin{code}
data MetaData h f = MetaData { mdHolder :: Maybe (Holder h)
                             , mdFrame  :: f
                             }
\end{code}

We also introduce a special \emph{type class} called \emph{ToBinomial}
which allows us to define a range of types that can be converted to a
binomial opinion. An example of such a type could be a \emph{Beta
PDF}. We will re-use this strategy for implementing multinomial and
hyper opinions.

\begin{code}
class ToBinomial op where
  toBinomial :: op h a -> Binomial h a

instance ToBinomial Binomial where
  toBinomial = id
\end{code}










\subsection{Multinomial Opinions}

Multinomials are represented as records containing a \emph{BeliefVector} to represent the
amount of belief assigned to each element of the frame, a scalar rational number to
store the uncertainty mass, a \emph{BaseRateVector} which assigns each element in the frame
to a base rate, and a meta-data object.


\begin{code}
data Multinomial h a =
  Multinomial { mBelief      :: BeliefVector a
              , mUncertainty :: Rational
              , mBaseRate    :: BaseRateVector a
              , mMetaData    :: MetaData h (F.Frame a)
              }
\end{code}

Just as in the case of binomials, we introduce a type class to represent types that can be
converted to multinomials. We provide the instance for multinomial opinions (the identity
function) as well as an instance for binomial opinions, since binomial opinions are a
special case of multinomial opinions.

\begin{code}
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

There are certain operations that are common amongst all opinions.
One example of such operation is the \emph{probability expectation}:
for binomials, the probability expectation is a simple scalar, whereas
for multinomial and hyper opinions the probability expectation is a
vector over the frame of discernment, and the reduced powerset of the
frame, respectively.

\begin{code}
class Opinion op h a where
  type FrameType op h a       :: *
  type ExpectationType op h a :: *

  expectation :: op h a -> ExpectationType op h a
  frame       :: op h a -> FrameType op h a
\end{code}

In order to accomodate a function such as probability expectation that
returns a value of a different type depending on the type of the opinion,
we use an \emph{indexed type family}. For each opinion type, we associate
an "expectation type", which is the type one would obtain when querying the
probability expectation of the opinion. The instances for each of the three
opinion types follows.

\begin{code}
instance Opinion Binomial h a where
  type FrameType Binomial h a       = F.BinaryFrame a
  type ExpectationType Binomial h a = Rational

  expectation (Binomial b d u a _) = b + a * u
  frame (Binomial _ _ _ _ (MetaData _ frm)) = frm

--deriving instance Eq (FrameType Binomial h a)



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
\label{sec:belief-coarsening}


Coarsening is an operation that takes a hyper opinion and converts it
into a binomial opinion. The inputs are an arbitrary hyper opinion and
a subset of the frame of discernment for which the hyper opinion is
defined over. Coarsening is a two-stage operation: First the frame of
discernment is partitioned into two sets: the subset given as input,
and everything else. These two subsets, taken together as a set, form
a new binary frame with which the new binomial opinion will be defined
over. Secondly, the belief masses associated with elements of the
powerset of the original frame via the hyper opinion input are split
up and assigned to the elements of the new frame. The resulting belief
mass assignment preserves additivity, and thus the new binomial
opinion is valid. The operation for coarsening is given below.


\begin{code}
coarsen :: (ToHyper op, Ord a) => op h a -> F.Subframe a -> Binomial h a
coarsen op theta = Binomial b d u a undefined
  where
    b = sumSnd . V.elemsWhere subset         $ belief
    d = sumSnd . V.elemsWhere emptyIntersect $ belief
    u = 1 - b - d
    a = sum . F.toList . F.map baseRate      $ theta

    belief   = hBelief . toHyper $ op
    baseRate = V.value (hBaseRate . toHyper $ op)

    sumSnd         = sum . map snd
    subset         = (`F.isSubsetOf` theta)
    emptyIntersect = F.isEmpty . (`F.intersection` theta)

\end{code}


As a convenience, we also offer a function to coarsen a hyper opinion,
not by an explicitly given subframe, but by those elements of the frame
that satisfy a given predicate. As an example, consider a frame of
discernment consisting of the natural numbers. Using the following
function, one can coarsen the frame into the binary frame
$\lbrace \mbox{evens}, \lnot \mbox{evens} \rbrace$.


\begin{code}
coarsenBy :: (ToHyper op, Ord a) => op h a -> (a -> Bool) -> Binomial h a
coarsenBy op pred = coarsen op theta
  where
    (theta, _) = F.partition pred . frame . toHyper $ op
\end{code}




\begin{code}
{-
sameFrame :: (Opinion op h a)
             => SLExpr h a (op h a) -> SLExpr h a (op h a)
             -> SLExpr h a Bool
sameFrame op1 op2 = do frm1 <- fmap frame op1
                       frm2 <- fmap frame op2
                       return (frm1 == frm2)
-}
\end{code}


\end{document}
