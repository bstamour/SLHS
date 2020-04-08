\documentclass[thesis.tex]{subfiles}

\begin{document}

\ignore{
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Math.SLHS.Frame where

import Data.List (subsequences, intercalate)
import qualified Data.Set as S
\end{code}
}


\subsection{Frames of Discernment}

We represent the frame of discernment as a container type that supports
set-like operations such as union and intersection. The reason that we
provide our own implementation instead of relying solely on the
Set data type provided by Haskell is to allow for future modifications
to the SLHS library to swap the underlying data structure, either for
performance reasons, or for portability.

We first introduce a new type representing a frame of discernment:

\begin{code}
newtype Frame a = Frame (S.Set a) deriving (Eq, Ord)
\end{code}

\ignore{
\begin{code}
instance Show a => Show (Frame a) where
  show (Frame s) = "{" ++ stuff ++ "}"
    where
      stuff = intercalate "," . Prelude.map show $ S.toList s
\end{code}
}

By declaring this type using Haskell's \emph{newtype} keyword, we are
actually creating a kind of strongly discriminating type alias. That is,
representationally Frame a is the same as Set a, however one cannot use
a frame when expecting a set, and vice versa.

We then expose the set-theoretic operators that are required by
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

difference :: Ord a => Frame a -> Frame a -> Frame a
difference (Frame s1) (Frame s2) = Frame (s1 S.\\ s2)
\end{code}


\begin{code}
partition :: (a -> Bool) -> Frame a -> (Frame a, Frame a)
partition p (Frame s) = let (s1, s2) = S.partition p s
                        in (Frame s1, Frame s2)

partitionMany :: [a -> Bool] -> Frame a -> [Frame a]
partitionMany [] frm = [frm]
partitionMany (p:ps) frm = let (f1, f2) = partition p frm
                           in f1 : partitionMany ps f2

size :: Frame a -> Int
size (Frame s) = S.size s

map :: (Ord a, Ord b) => (a -> b) -> Frame a -> Frame b
map f (Frame s) = Frame (S.map f s)

fold :: (a -> b -> b) -> b -> Frame a -> b
fold f z (Frame s) = S.fold f z s

toList :: Frame a -> [a]
toList (Frame s) = S.toList s

fromList :: Ord a => [a] -> Frame a
fromList xs = Frame $ S.fromList xs

singleton :: Ord a => a -> Frame a
singleton x = fromList [x]

member :: Ord a => a -> Frame a -> Bool
member x (Frame s) = x `S.member` s

powerSet :: Ord a => Frame a -> Frame (Frame a)
powerSet (Frame s) = fromList frames
  where
    frames = Prelude.map fromList (subsequences (S.toList s))

reducedPowerSet :: Ord a => Frame a -> Frame (Frame a)
reducedPowerSet frm@(Frame s) = Frame $ S.map Frame rpset'
  where
    (Frame pset) = powerSet frm
    pset' = S.map (\(Frame x) -> x) pset
    rpset = pset' S.\\ S.fromList [S.empty]
    rpset' = rpset S.\\ S.fromList [s]

cross :: (Ord a, Ord b) => Frame a -> Frame b -> Frame (a, b)
cross (Frame s1) (Frame s2) = fromList [ (x, y) | x <- S.toList s1, y <- S.toList s2 ]
\end{code}

The \emph{cross} function computes the cartesian product of two frames, and the functions
\emph{powerSet} and \emph{reducedPowerSet} compute the powerSet and reduced powerSet of
the input frame.

\end{document}
