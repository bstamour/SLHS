

\ignore{
\begin{code}
module Math.SLHS.BeliefVector where

import Math.SLHS.Core
import Math.SLHS.Frame
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M
\end{code}
}


\begin{code}
data BeliefVector a = BeliefVector { bvFrame :: Frame a
                                   , bvMap   :: M.Map (Subframe a) Rational
                                   }
\end{code}


\begin{code}
mkBeliefVector :: Ord a => Frame a -> [(Subframe a, Rational)] -> SLVal (BeliefVector a)
mkBeliefVector f pairs = do
  let allSubsets = all ((`isSubsetOf` f) . fst) pairs
  require allSubsets "Must all be subsets"
  return . BeliefVector f $ M.fromList pairs
\end{code}


\begin{code}
getBeliefMass :: Ord a => BeliefVector a -> Subframe a -> SLVal Rational
getBeliefMass (BeliefVector f m) sf = do
  require (sf `isSubsetOf` f) "fucker"
  return . fromMaybe 0 $ M.lookup sf m
\end{code}
