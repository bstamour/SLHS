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
import Control.Monad (liftM2, join)
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
add' :: (ToBinomial op1, ToBinomial op2)
        => op1 a -> op2 a -> SLExpr a (Binomial a)
add' = undefined

subtract' :: (ToBinomial op1, ToBinomial op2)
             => op1 a -> op2 a -> SLExpr a (Binomial a)
subtract' = undefined

multiply' :: (ToBinomial op1, ToBinomial op2)
             => op1 a -> op2 a -> SLExpr a (Binomial a)
multiply' = undefined

divide' :: (ToBinomial op1, ToBinomial op2)
           => op1 a -> op2 a -> SLExpr a (Binomial a)
divide' = undefined

comultiply' :: (ToBinomial op1, ToBinomial op2)
               => op1 a -> op2 a -> SLExpr a (Binomial a)
comultiply' = undefined

codivide' :: (ToBinomial op1, ToBinomial op2)
             => op1 a -> op2 a -> SLExpr a (Binomial a)
codivide' = undefined
\end{code}

As the type signatures show, the innermost functions compute over binomials, without
mention of any monadic context. However the return value is in fact monadic. This is
because these functions can update the internal memoization table.

We then lift the above functions into the monad like so

\begin{code}
add :: (ToBinomial op1, ToBinomial op2)
       => SLExpr a (op1 a) -> SLExpr a (op2 a) -> SLExpr a (Binomial a)
add = wrap2 add'

subtract :: (ToBinomial op1, ToBinomial op2)
            => SLExpr a (op1 a) -> SLExpr a (op2 a) -> SLExpr a (Binomial a)
subtract = wrap2 subtract'

multiply :: (ToBinomial op1, ToBinomial op2)
            => SLExpr a (op1 a) -> SLExpr a (op2 a) -> SLExpr a (Binomial a)
multiply = wrap2 multiply'

divide :: (ToBinomial op1, ToBinomial op2)
          => SLExpr a (op1 a) -> SLExpr a (op2 a) -> SLExpr a (Binomial a)
divide = wrap2 divide'

comultiply :: (ToBinomial op1, ToBinomial op2)
              => SLExpr a (op1 a) -> SLExpr a (op2 a) -> SLExpr a (Binomial a)
comultiply = wrap2 multiply'

codivide :: (ToBinomial op1, ToBinomial op2)
            => SLExpr a (op1 a) -> SLExpr a (op2 a) -> SLExpr a (Binomial a)
codivide = wrap2 divide'
\end{code}

where \emph{wrap2} is a helper function, defined as follows:

\begin{code}
wrap2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
wrap2 f ma mb = join $ liftM2 f ma mb
\end{code}

\subsection{Multinomial Operators}

\begin{code}
m_multiply' :: Multinomial a -> Multinomial a -> SLExpr (SLState a) (Multinomial a)
m_multiply' = undefined
\end{code}




\end{document}
