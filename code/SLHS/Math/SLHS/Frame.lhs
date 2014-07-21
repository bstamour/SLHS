\documentclass[thesis.tex]{subfiles}

\begin{document}

\ignore{
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Math.SLHS.Frame where

import qualified Data.Set as S
\end{code}
}


\subsection{Frames of Discernment}

We represent the frame of discernment as a data type that supports
set-like operations such as union and intersection. The reason that we
provide our own implementation instead of relying solely on the
Set data type provided by Haskell is to allow for future modifications
to the SLHS library to swap the underlying data structure, either for
performance reasons, or for portability.

We first introduce a new type representing a frame of discernment:

\begin{code}
newtype Frame a = Frame (S.Set a) deriving (Eq, Ord)
\end{code}

By declaring this type using Haskell's \emph{newtype} keyword, we are
actually creating a kind of strongly discriminating type alias. That is,
representationally Frame a is the same as Set a, however one cannot use
a frame when expecting a set, and vice versa.

In order to allow for the types of our operators to be more descriptive,
we introduce a type alias Subframe, which while is the same as a frame,
should only be used in places where one explicitly requires the object to
be a subset of an existing frame of discernment.

\begin{code}
type Subframe a = Frame a
\end{code}

\begin{code}
data BinaryFrame a = BinaryFrame { bfX :: a, bfX' :: a }
\end{code}



\begin{code}
class ToFrame f a where
  toFrame :: f a -> Frame a

instance ToFrame Frame a where
  toFrame = id

instance Ord a => ToFrame BinaryFrame a where
  toFrame (BinaryFrame x y) = Frame (S.fromList [x, y])
\end{code}

Finally, we expose those set-theoretic operators that are required by
the rest of the library implementation.

\begin{code}
empty :: Frame a
empty = Frame (S.empty)

isEmpty :: Eq a => Frame a -> Bool
isEmpty f = f == empty

union :: Ord a => Frame a -> Frame a -> Frame a
union (Frame s1) (Frame s2) = Frame (s1 `S.union` s2)

isSubsetOf :: Ord a => Frame a -> Frame a -> Bool
isSubsetOf (Frame s1) (Frame s2) = s1 `S.isSubsetOf` s2

intersection :: Ord a => Frame a -> Frame a -> Frame a
intersection (Frame s1) (Frame s2) = Frame (s1 `S.intersection` s2)
\end{code}


\begin{code}
partition :: (a -> Bool) -> Frame a -> (Frame a, Frame a)
partition p (Frame s) = let (s1, s2) = S.partition p s
                        in (Frame s1, Frame s2)

partitionMany :: [a -> Bool] -> Frame a -> [Frame a]
partitionMany [] frm = [frm]
partitionMany (p:ps) frm = let (f1, f2) = partition p frm
                           in f1 : partitionMany ps f2

map :: (Ord a, Ord b) => (a -> b) -> Frame a -> Frame b
map f (Frame s) = Frame (S.map f s)

toList :: Frame a -> [a]
toList (Frame s) = S.toList s
\end{code}


\end{document}
