\documentclass[thesis.tex]{subfiles}

\begin{document}

\section{Opinions}

\ignore{
\begin{code}
module Math.SLHS.Opinions where

import Math.SLHS.Base
\end{code}
}

\subsection{Binomial Opinions}

Binomial opinions are represented by a record of four rational numbers,
an element labelled $x$, and an element labelled $\lnot x$.

\begin{code}
data Binomial a =
  Binomial { binBelief      :: Rational
           , binDisbelief   :: Rational
           , binUncertainty :: Rational
           , binBaseRate    :: Rational
           , binX           :: a
           , binNotX        :: a
           }
\end{code}

We also introduce a special \emph{type class} called \emph{ToBinomial} which allows
us to define a range of types that can be converted to a binomial opinion. An example
of such a type could be a \emph{Beta PDF}.

\begin{code}
class ToBinomial op where
  toBinomial :: op a -> Binomial a
\end{code}

\subsection{Multinomial Opinions}

\begin{code}
data Multinomial a =
  Multinomial { mulBelief      :: BeliefVector a
              , mulUncertainty :: Rational
              , mulBaseRate    :: BaseRateVector a
              , mulFrame       :: Frame a
              }

class ToMultinomial op where
  toMultinomial :: op a -> Multinomial a
\end{code}


\subsection{Hyper Opinions}

\begin{code}
data Hyper a = Hyper a

class ToHyper op where
  toHyper :: op a -> Hyper a
\end{code}


\begin{code}
class Opinion op where
  expectation :: op a -> Rational

instance Opinion Binomial where
  expectation (Binomial b d u a _ _) = b + a * u
\end{code}


\end{document}
