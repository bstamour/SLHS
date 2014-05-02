\documentclass[thesis.tex]{subfiles}

\begin{document}

\ignore{
\begin{code}
module Math.SLHS.Operators
       ( add
       , subtract
       , multiply
       , divide
       , comultiply
       , codivide
       ) where

import Prelude hiding (subtract)
import Math.SLHS.Core
import Math.SLHS.Opinions
\end{code}
}

\section{Operators}

\subsection{Binomial Operators}

We begin this section by describing the operators defined over \emph{Binomial opinions}.
Many of these operators have equivalents in standard logic or set theory. The operators
are defined in layers: the innermost layer contains the actual code to compute the
resulting values, and the outermost layer \emph{lifts} the operators to work within
the monadic context.

\begin{code}
add' :: Binomial a -> Binomial a -> SLExpr (SLState a) (Binomial a)
add' = undefined

subtract' :: Binomial a -> Binomial a -> SLExpr (SLState a) (Binomial a)
subtract' = undefined

multiply' :: Binomial a -> Binomial a -> SLExpr (SLState a) (Binomial a)
multiply' = undefined

divide' :: Binomial a -> Binomial a -> SLExpr (SLState a) (Binomial a)
divide' = undefined

comultiply' :: Binomial a -> Binomial a -> SLExpr (SLState a) (Binomial a)
comultiply' = undefined

codivide' :: Binomial a -> Binomial a -> SLExpr (SLState a) (Binomial a)
codivide' = undefined
\end{code}

As the type signatures show, the innermost functions compute over binomials, without
mention of any monadic context. However the return value is in fact monadic. This is
because these functions can update the internal memoization table.

We then lift the above functions into the monad like so

\begin{code}
add        = wrap2 add'
subtract   = wrap2 subtract'
multiply   = wrap2 multiply'
divide     = wrap2 divide'
comultiply = wrap2 multiply'
codivide   = wrap2 divide'
\end{code}

where \emph{wrap2} is a helper function, defined as follows:

\begin{code}
wrap2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
wrap2 f ma mb = ma >>= \a -> mb >>= \b -> f a b
\end{code}


\end{document}
