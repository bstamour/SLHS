\documentclass[thesis.tex]{subfiles}

\begin{document}

\section{Opinions}

\ignore{
\begin{code}
module Math.SLHS.Opinions where

import Math.SLHS.Types
import qualified Data.Set as S
\end{code}
}







\subsection{Binomial Opinions}


We represent binomial opnions by four rational numbers corresponding to the belief,
disbelief, uncertainty, and base rate of the opinion, along with some additional
book-keeping data: Binomial opinions may have an optional \emph{belief holder}, and
every binomial opinion must contain an object for which the opinion is held over.
The definition of the binomial opinion is thus


\begin{code}
data Binomial s o =
  Binomial { binBelief      :: Rational
           , binDisbelief   :: Rational
           , binUncertainty :: Rational
           , binBaseRate    :: Rational
           , binSubject     :: Maybe (Holder s)
           , binObject      :: BinomialObject o
           }
\end{code}


We define the binomial object as a separate data type that captures the three kinds
of objects which we allow binomial opinions to rein over, namely:

\begin{itemize}
\item A single predicate $x$, and it's negation $\lnot x$, which together form a
binary frame,
\item A binary partitioning of a frame with cardinality $N > 2$,
\item Another belief holder.
\end{itemize}

The third case is useful for modelling trust-transitivity networks, where agents can
construct beliefs of events through the recommendation of other agents.



\begin{code}
data BinomialObject o = Predicate o o
                      | Partition (Subframe o) (Subframe o)
                      | Person (Holder o)
\end{code}




We also introduce a special \emph{type class} called \emph{ToBinomial} which allows
us to define a range of types that can be converted to a binomial opinion. An example
of such a type could be a \emph{Beta PDF}.




\begin{code}
class ToBinomial op where
  toBinomial :: op s o -> Binomial s o
\end{code}


\subsection{Multinomial Opinions}


\begin{code}
type BaseRateVector = BeliefVector

data Multinomial s o  =
  Multinomial { mulBelief      :: BeliefVector o
              , mulUncertainty :: Rational
              , mulBaseRate    :: BaseRateVector o
              , mulSubject     :: Maybe (Holder s)
              , mulObject      :: Frame o
              }


class ToMultinomial op where
  toMultinomial :: op s o -> Multinomial s o
\end{code}


\subsection{Hyper Opinions}


\begin{code}
data Hyper a = Hyper a


class ToHyper op where
  toHyper :: op a -> Hyper a
\end{code}


\begin{code}
class Opinion op where
  expectation :: op s o -> Rational
  frame       :: Ord o => op s o -> Frame o


instance Opinion Binomial where
  expectation (Binomial b d u a _ _) = b + a * u

  frame (Binomial _ _ _ _ _ obj) = frm obj
    where
      frm (Predicate x y)     = S.fromList [x, y]
      frm (Partition xs ys)   = xs `S.union` ys
      frm (Person (Holder p)) = S.singleton p
\end{code}


\end{document}
