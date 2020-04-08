\documentclass[thesis.tex]{subfiles}

\begin{document}



\subsection{Belief Vectors}

We introduce a special type for representing belief vectors - containers
whose elements are belief masses. The reason for introducing a new
type instead of simply re-using an existing container type is so that
in the future if analysis proves that a different container type
provides more efficient operations, then the internal represent of our
belief vectors can be changed without affecting any other portion of
the SLHS code-base. For the time being we have chosen to use Haskell's
Map data type, which is a key-value store backed by an efficient
red-black tree. It guarantees $O(\log_2 n)$ time for looking up individual
elements, and allows us to traverse the entire tree in $O(n)$ time.
thus leads to very efficient Subjective Logic operators.


\ignore{
\begin{code}
module Math.SLHS.Vector where

import Data.List (intercalate)
import Data.Maybe
import qualified Data.Map as M
\end{code}
}

We start with the definition of the Vector type.

\begin{code}
newtype Vector a = Vector { unVec :: M.Map a Rational }
\end{code}

\ignore{
\begin{code}
instance Show a => Show (Vector a) where
  show (Vector m) = "<" ++ stuff ++ ">"
    where
      stuff = intercalate "," . Prelude.map show $ M.toList m
\end{code}
}

Next we introduce some functions for converting belief vectors
to and from standard Haskell lists.

\begin{code}
fromList :: Ord a => [(a, Rational)] -> Vector a
fromList = Vector . M.fromList

toList :: Vector a -> [(a, Rational)]
toList = M.toList . unVec
\end{code}

Finally, we introduce functions for interfacing with vectors.

\begin{code}
value :: Ord a => Vector a -> a -> Rational
value v x = fromMaybe 0 . M.lookup x $ unVec v

map :: (Rational -> Rational) -> Vector a -> Vector a
map f = Vector . M.map f . unVec

mapWithKey :: (a -> Rational -> Rational) -> Vector a -> Vector a
mapWithKey f = Vector . M.mapWithKey f . unVec

fold :: (Rational -> b -> b) -> b -> Vector a -> b
fold f z = M.fold f z . unVec

focals :: Vector a -> [a]
focals = M.keys . unVec

elemsWhere :: (a -> Bool) -> Vector a -> [(a, Rational)]
elemsWhere p = filter (\(k, _) -> p k) . toList
\end{code}

\emph{value} retrieves the value associated with a particular key. \emph{map} allows us to apply a function over each value,
returning a new transformed vector. The \emph{mapWithKey} function allows us to map a function over the vector that takes the key into account.
\emph{fold} allows us to accumulate a vector into a single value by applying an operator
between each element. \emph{focals} returns a list of keys that have non-zero mass. Lastly, \emph{elemsWhere} returns a list of
key-value pairs, where the key satisfies a certain predicate.


\end{document}
