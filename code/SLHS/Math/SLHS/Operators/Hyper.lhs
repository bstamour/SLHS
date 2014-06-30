\documentclass[thesis.tex]{subfiles}

\begin{document}


\ignore{
\begin{code}
module Math.SLHS.Operators.Hyper where

import Math.SLHS.Types
import Math.SLHS.Opinions

import qualified Math.SLHS.Vector as V

import Control.Monad (liftM2, join)
import Control.Applicative
import Data.Maybe
import Data.List

import qualified Data.Map as M
\end{code}
}






\subsection{Hyper Operators}

We now present the most general operators of Subjective Logic: those whose
inputs are hyper opinions.



\subsubsection{Cumulative Fusion}



\begin{code}
cFuse :: (ToHyper op1, ToHyper op2, Ord a)
         => SLExpr h a (op1 h a) -> SLExpr h a (op2 h a) -> SLExpr h a (Hyper h a)
cFuse opa opb = do opa' <- toHyper <$> opa
                   opb' <- toHyper <$> opb
                   pure $ cFuse' opa' opb'
\end{code}


\begin{code}
cFuse' :: Ord a => Hyper h a -> Hyper h a -> Hyper h a
cFuse' (Hyper ba ua aa _) (Hyper bb ub ab _)
  | ua /= 0 || ub /= 0 = Hyper b' u' a' undefined
  | otherwise          = Hyper b'' u'' a'' undefined
  where
    b' = V.fromList . map (\k -> (k, bFunc k)) $ keys
    u' = ua * ub / (ua + ub - ua * ub)
    a' = aa

    b'' = V.fromList . map (\k -> (k, bB k)) $ keys
    u'' = 0
    a'' = aa

    bFunc x = (bA x * ub + bB x * ua) / (ua + ub - ua * ub)

    keys  = nub (V.focals ba ++ V.focals bb)

    bA = V.value ba
    bB = V.value bb
\end{code}






\subsubsection{Averaging Fusion}


\begin{code}
aFuse :: (ToHyper op1, ToHyper op2, Ord a)
         => SLExpr h a (op1 h a) -> SLExpr h a (op2 h a) -> SLExpr h a (Hyper h a)
aFuse opa opb = do opa' <- toHyper <$> opa
                   opb' <- toHyper <$> opb
                   pure $ aFuse' opa' opb'
\end{code}



\begin{code}
aFuse' :: Ord a => Hyper h a -> Hyper h a -> Hyper h a
aFuse' (Hyper ba ua aa _) (Hyper bb ub ab _)
  | ua /= 0 || ub /= 0 = Hyper b' u' a' undefined
  | otherwise          = Hyper b'' u'' a'' undefined
  where
    b' = V.fromList . map (\k -> (k, bFunc k)) $ keys
    u' = 2 * ua * ub / (ua + ub)
    a' = aa

    b'' = V.fromList . map (\k -> (k, bB k)) $ keys
    u'' = 0
    a'' = aa

    bFunc x = (bA x * ub + bB x * ua) / (ua + ub)

    keys  = nub (V.focals ba ++ V.focals bb)

    bA = V.value ba
    bB = V.value bb
\end{code}







\subsubsection{Belief Constraining}


\begin{code}
constraint :: (ToHyper op1, ToHyper op2, Ord a)
              => SLExpr h a (op1 h a)
              -> SLExpr h a (op2 h a)
              -> SLExpr h a (Hyper h a)
constraint op1 op2 = constraint' <$> (fmap toHyper op1) <*> (fmap toHyper op2)
\end{code}


\begin{code}
constraint' :: Ord a => Hyper h a -> Hyper h a -> Hyper h a
constraint' (Hyper bA uA aA _) (Hyper bB uB aB _) = Hyper bAB uAB aAB undefined
  where
    bAB = V.fromList . map (\k -> (k, harmony k / (1 - conflict))) $ keys
    uAB = (uA * uB) / (1 - conflict)
    aAB = V.fromList $ map (\k -> (k, f k)) keys'
      where
        f x = (axA * (1 - uA) + axB * (1 - uB)) / (2 - uA - uB)
          where
            axA = V.value aA x
            axB = V.value aB x

    harmony x = bxA * uB + bxB * uA + rest
      where
        bxA     = V.value bA x
        bxB     = V.value bB x
        rest    = sum . map combine $ matches
        matches = [(y, z) | y <- keys, z <- keys, fUnion y z == x]

    conflict = sum . map combine $ matches
      where
        matches = [(y, z) | y <- keys, z <- keys, fUnion y z == fEmpty]

    combine (y, z) = V.value bA y + V.value bB z

    keys  = nub (V.focals bA ++ V.focals bB)
    keys' = nub (V.focals aA ++ V.focals aB)
\end{code}


\end{document}
