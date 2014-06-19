\documentclass[thesis.tex]{subfiles}


\begin{document}

\ignore{
\begin{code}
module Math.SLHS.Operators.Multinomial where

import Math.SLHS.Opinions
import Math.SLHS.Types

import Data.Maybe
import Data.List
import Control.Applicative
import qualified Data.Map as M
\end{code}
}


\subsection{Multinomial Operators}


\subsubsection{Multinomial Multiplication}


% TODO: Implement me. Make sure the types are correct.

\begin{code}
times' :: Multinomial h a -> Multinomial h a -> Multinomial h a
times' = undefined
\end{code}




\subsubsection{Multinomial Unfusion}


\begin{code}
cUnfuse' :: Multinomial h a -> Multinomial h a -> Multinomial h a
cUnfuse' opC opb = undefined -- Compute opa
\end{code}





\subsubsection{Multinomial Fission}


\begin{code}
cSplit :: ToMultinomial op => Rational -> SLExpr h a (op h a)
          -> SLExpr h a (Multinomial h a , Multinomial h a)
cSplit phi op = cSplit' <$> (pure phi) <*> (toMultinomial <$> op)
\end{code}


\begin{code}
cSplit' :: Rational -> Multinomial h a -> (Multinomial h a, Multinomial h a)
cSplit' phi (Multinomial b u a _) = (op1, op2)
  where
    op1 = Multinomial b1 u1 a undefined
    op2 = Multinomial b2 u2 a undefined

    b1 = M.map (\x -> phi * x / norm phi) b
    u1 = u / norm phi

    b2 = M.map (\x -> (1 - phi) * x / norm (1 - phi)) b
    u2 = u / norm (1 - phi)

    norm p = u + p * M.fold (+) 0 b
\end{code}







\end{document}
