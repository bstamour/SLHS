

\begin{code}
module Math.SLHS.Frame where

import qualified Data.Set as S

newtype Frame a = Frame { unFrame :: S.Set a } deriving (Eq, Ord)

type Subframe a = Frame a

union :: Ord a => Frame a -> Frame a -> Frame a
union (Frame xs) (Frame ys) = Frame $ xs `S.union` ys

isSubsetOf :: Ord a => Frame a -> Frame a -> Bool
isSubsetOf (Frame xs) (Frame ys) = xs `S.isSubsetOf` ys

\end{code}
