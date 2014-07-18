\documentclass[thesis.tex]{subfiles}

\begin{document}



\section{Extensions to Subjective Logic}





\ignore{
\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Math.SLHS.Extensions where

import Math.SLHS.Types
import Math.SLHS.Opinions
import qualified Math.SLHS.Vector as V
import qualified Math.SLHS.Frame as F

import qualified Data.Set as S
\end{code}
}



\subsection{Hypernomial to Multinomial Coarsening}


\begin{code}
hyperCoarsen :: ToHyper op => op h a -> [F.Subframe a] -> Multinomial h (F.Subframe a)
hyperCoarsen op thetas = Multinomial b u a undefined
  where
    b = undefined
    u = undefined
    a = undefined
\end{code}



\end{document}
