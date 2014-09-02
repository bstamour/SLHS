\documentclass[thesis.tex]{subfiles}

\begin{document}


\section{Extensions to Subjective Logic}
\label{sec:extensions-to-sl}


In this section we describe new Subjective Logic operators that do not yet appear in
the published literature. While Subjective Logic contains a wealth of operators for
reasoning with uncertainty \cite{josang2008conditional, josang2008abductive}, modeling
transitive trust networks \cite{josang2001logic}, and analyzing hypotheses \cite{pope2005analysis},
the set of all theoretically possible operators is incomplete. If we assume that binomial
opinions alone are represented as four 32-bit numbers, then the set of all possible unique
operators for binomials would be of cardinality
$2^{32} \times 2^{32} = 2^{64} = 18446744073709551616$. Whether any or all of these additional
operators are meaningful is up to interpretation, of course.


\ignore{
\begin{code}
module Math.SLHS.Extensions where

import Math.SLHS.Types
import Math.SLHS.Opinions
import qualified Math.SLHS.Vector as V
import qualified Math.SLHS.Frame as F

import Data.Ratio ((%))
import qualified Data.Set as S
\end{code}
}


\subsection{Hypernomial to Multinomial Coarsening}

The first extension to the set of Subjective Logic operators we present is a generalized
form of coarsening discussed in section~\ref{sec:belief-coarsening}. Currently coarsening
is defined to be an operation from multinomials to binomials where a subset of the frame
of discernment is chosen to be a new element in a binary frame, and the remaining elements
of the frame are taken to be the second element, or the \emph{not} of the first element.
We generalize this operation to allow for arbitrary hyper opinions to be coarsened into
multinomial opinions defined over frames of cardinality $N \geq 2$.

\begin{code}
hyperCoarsen :: (Ord a, ToHyper op)
                => op h a -> [F.Frame a] -> Multinomial h (F.Frame a)
hyperCoarsen op thetas = Multinomial b' u' a' h f'
  where
    (Hyper b u a h f) = toHyper op

    b' = V.fromList [ (t, bel t) | t <- thetas ]
    u' = 1 - V.fold (+) 0 b'
    a' = V.fromList [ (t, br t / norm) | t <- thetas ]
    f' = F.fromList thetas

    norm = sum [ br t | t <- thetas ]

    bel = sum . map snd . overlaps b
    br  = F.fold (+) 0 . F.map (V.value a)

    overlaps v t = V.elemsWhere (\u -> u `F.isSubsetOf` t) v
\end{code}

Given a hyper opinion and a list of frames of discernment, we construct a new multinomial
opinion over a new frame made up of frames as elements. Focal elements that are contained
entirely within one of the listed frames contribute their belief mass to the new multinomial
opinion, and the remaining mass is lumped into the uncertainty. The new base rates are simply
the sums of the base rates multiplied by the normalizing constant

$$
\frac{1}{\sum_{t \in Thetas} \sum_{x \in t} a(x)}
$$

where $a(x)$ is the base rate of $x$ from the input
hyper opinion.

We do not claim that this is the only method that one could use to coarsen a hyper opinion
to a multinomial opinion. We present this as simply one method that one could employ.


\subsection{Uncoarsening from Binomial to Multinomial}

In the case of when a binomial opinion is defined over a binary partitioning of a frame,
we can uncoarsen it into a multinomial opinion with the following procedure:

\begin{code}
uncoarsen :: Ord a => Binomial h (F.Frame a) -> Multinomial h a
uncoarsen (Binomial b d u a h xs ys) = Multinomial b' u a' h f
  where
    f = xs `F.union` ys
    b' = V.fromList $
           [ (x, r) | x <- F.toList xs, let r = b / toRational (F.size xs) ]
           ++
           [ (y, r) | y <- F.toList ys, let r = d / toRational (F.size ys) ]
    a' = V.fromList $
           [ (x, r) | x <- F.toList xs, let r = a / toRational (F.size xs) ]
           ++
           [ (y, r) | y <- F.toList ys, let r = (1 - a) / toRational (F.size ys) ]
\end{code}

\begin{comment}
In Section \ref{sec:multinomial-opinions} we introduced a Haskell Type Class called
\emph{ToMultinomial}. Types who are members of this class must provide a function
called \emph{toMultinomial} which acts as a conversion from the particular type to
a multinomial opinion. In the implementation of \emph{toMultinomial} for binomial opinions
we mentioned that the operation can be seen as a kind of \emph{uncoarsening} when the
binomial opinion is defined over a partitioned frame of discernment. For completeness,
we repeat the relevant code here:

\begin{spec}
instance ToMultinomial Binomial where
  toMultinomial (Binomial b d u a h (Split xs ys)) = Multinomial b' u a' h f
    where
      b' = V.fromList $
           [ (x, r) | x <- F.toList xs, let r = b / toRational (F.size xs) ]
           ++
           [ (y, r) | y <- F.toList ys, let r = d / toRational (F.size ys) ]
      a' = V.fromList $
           [ (x, r) | x <- F.toList xs, let r = a / toRational (F.size xs) ]
           ++
           [ (y, r) | y <- F.toList ys, let r = (1 - a) / toRational (F.size ys) ]
      f  = xs `F.union` ys
\end{spec}
\end{comment}

\end{document}
