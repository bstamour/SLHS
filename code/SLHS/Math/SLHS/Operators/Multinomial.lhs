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


\begin{code}
m_multiply' :: Multinomial s o -> Multinomial s o -> SLExpr h o (Multinomial s o)
m_multiply' = undefined
\end{code}




\subsubsection{Cumulative Fusion}


% Note: yA = 0, yB = 1, if you follow through with the limits.


TODO: The base rate vectors should be the same.

TODO: The frames must be the same as well.


\begin{code}
cFuse :: (ToMultinomial op1, ToMultinomial op2, Ord a)
         => SLExpr h a (op1 h a) -> SLExpr h a (op2 h a)
         -> SLExpr h a (Multinomial h a)
cFuse opa opb = do opa' <- toMultinomial <$> opa
                   opb' <- toMultinomial <$> opb
                   pure $ cFuse' opa' opb'
\end{code}


\begin{code}
cFuse' :: Ord a => Multinomial h a -> Multinomial h a -> Multinomial h a
cFuse' (Multinomial ba ua aa _) (Multinomial bb ub ab _)
  | ua /= 0 || ub /= 0 = Multinomial b' u' a' undefined
  | otherwise          = Multinomial b'' u'' a'' undefined
  where
    b' = M.fromList . map (\k -> (k, bFunc k)) $ keys
    u' = ua * ub / (ua + ub - ua * ub)
    a' = aa

    b'' = M.fromList . map (\k -> (k, bB k)) $ keys
    u'' = 0
    a'' = aa

    bFunc x = (bA x * ub + bB x * ua) / (ua + ub - ua * ub)

    keys  = nub (M.keys ba ++ M.keys bb)

    bA = lookup' ba
    bB = lookup' bb
\end{code}


\subsubsection{Averaging Fusion}



\begin{code}
aFuse :: (ToMultinomial op1, ToMultinomial op2, Ord a)
         => SLExpr h a (op1 h a) -> SLExpr h a (op2 h a)
         -> SLExpr h a (Multinomial h a)
aFuse opa opb = do opa' <- toMultinomial <$> opa
                   opb' <- toMultinomial <$> opb
                   pure $ aFuse' opa' opb'
\end{code}



\begin{code}
aFuse' :: Ord a => Multinomial h a -> Multinomial h a -> Multinomial h a
aFuse' (Multinomial ba ua aa _) (Multinomial bb ub ab _)
  | ua /= 0 || ub /= 0 = Multinomial b' u' a' undefined
  | otherwise          = Multinomial b'' u'' a'' undefined
  where
    b' = M.fromList . map (\k -> (k, bFunc k)) $ keys
    u' = 2 * ua * ub / (ua + ub)
    a' = aa

    b'' = M.fromList . map (\k -> (k, bB k)) $ keys
    u'' = 0
    a'' = aa

    bFunc x = (bA x * ub + bB x * ua) / (ua + ub)

    keys  = nub (M.keys ba ++ M.keys bb)

    bA = lookup' ba
    bB = lookup' bb
\end{code}


\end{document}
