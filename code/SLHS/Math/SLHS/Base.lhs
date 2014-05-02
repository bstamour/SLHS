\documentclass[thesis.tex]{subfiles}

\begin{document}


\ignore{
\begin{code}
module Math.SLHS.Base
       ( Frame()
       , BeliefVector()
       , BaseRateVector()
       ) where

import qualified Data.Set as S
import qualified Data.Map as M
\end{code}

\begin{code}
newtype Frame a = Frame (S.Set a)

newtype BeliefVector a = BeliefVector (M.Map a Rational)

newtype BaseRateVector a = BaseRateVector (M.Map a Rational)
\end{code}

\end{document}
