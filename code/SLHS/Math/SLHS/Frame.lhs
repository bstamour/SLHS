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


\begin{code}
newtype Frame a = Frame (S.Set a) deriving (Eq, Ord)
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

map :: (Ord a, Ord b) => (a -> b) -> Frame a -> Frame b
map f (Frame s) = Frame (S.map f s)

toList :: Frame a -> [a]
toList (Frame s) = S.toList s
\end{code}


\end{document}
