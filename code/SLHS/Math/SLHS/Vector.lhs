\documentclass[thesis.tex]{subfiles}

\begin{document}



\subsection{Belief Vectors}


\ignore{
\begin{code}
module Math.SLHS.Vector where

import Data.Maybe
import qualified Data.Map as M
\end{code}
}


\begin{code}
newtype Vector a = Vector { unVec :: M.Map a Rational }
\end{code}


\begin{code}
fromList :: Ord a => [(a, Rational)] -> Vector a
fromList = Vector . M.fromList


toList :: Vector a -> [(a, Rational)]
toList = M.toList . unVec
\end{code}


\begin{code}
value :: Ord a => Vector a -> a -> Rational
value v x = fromMaybe 0 . M.lookup x $ unVec v


map :: (Rational -> Rational) -> Vector a -> Vector a
map f = Vector . M.map f . unVec


fold :: (Rational -> b -> b) -> b -> Vector a -> b
fold f z = M.fold f z . unVec


focals :: Vector a -> [a]
focals = M.keys . unVec


elemsWhere :: (a -> Bool) -> Vector a -> [(a, Rational)]
elemsWhere p = filter (\(k, _) -> p k) . toList

\end{code}




\end{document}
