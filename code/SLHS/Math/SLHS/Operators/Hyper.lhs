\documentclass[thesis.tex]{subfiles}

\begin{document}


\ignore{
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}


module Math.SLHS.Operators.Hyper where

import Control.Monad (liftM2, join)
import Control.Applicative
import Data.Maybe
import Data.List
import qualified Data.Map as M

import Math.SLHS.Types
import Math.SLHS.Opinions
\end{code}
}


\subsection{Hyper Operators}





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
    bAB = M.fromList . map (\k -> (k, harmony k / (1 - conflict))) $ keys
    uAB = (uA * uB) / (1 - conflict)
    aAB = M.fromList $ map (\k -> (k, f k)) keys'
      where
        f x = (axA * (1 - uA) + axB * (1 - uB)) / (2 - uA - uB)
          where
            axA = lookup' aA x
            axB = lookup' aB x

    harmony x = bxA * uB + bxB * uA + rest
      where
        bxA     = lookup' bA x
        bxB     = lookup' bB x
        rest    = sum . map combine $ matches
        matches = [(y, z) | y <- keys, z <- keys, fUnion y z == x]

    conflict = sum . map combine $ matches
      where
        matches = [(y, z) | y <- keys, z <- keys, fUnion y z == fEmpty]

    combine (y, z) = lookup' bA y + lookup' bB z

    keys  = nub (M.keys bA ++ M.keys bB)
    keys' = nub (M.keys aA ++ M.keys aB)
\end{code}




\end{document}
