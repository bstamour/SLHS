\documentclass[thesis.tex]{subfiles}

\begin{document}


\section{Extensions to Subjective Logic}
\label{sec:extensions-to-sl}


In this section we describe new Subjective Logic operators that do not yet appear in
the literature.


\ignore{
\begin{code}
module Math.SLHS.Extensions where

import Math.SLHS.Types
import Math.SLHS.Opinions
import qualified Math.SLHS.Vector as V
import qualified Math.SLHS.Frame as F

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
hyperCoarsen :: ToHyper op => op h a -> [F.Subframe a] -> Multinomial h (F.Subframe a)
hyperCoarsen op thetas = Multinomial b u a undefined
  where
    b = undefined
    u = undefined
    a = undefined
\end{code}





We do not claim that this is the only method that one could use to coarsen a hyper opinion
to a multinomial opinion. We present this as simply one method that one could employ.



\end{document}
