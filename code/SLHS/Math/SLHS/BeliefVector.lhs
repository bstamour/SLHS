


\begin{code}
module Math.SLHS.BeliefVector where

import Math.SLHS.Frame
import qualified Data.Map as M

newtype BeliefVector a = BeliefVector { bvFrame :: Frame a
                                      , bvMap   :: M.Map (Frame a) Rational
                                      }

\end{code}
