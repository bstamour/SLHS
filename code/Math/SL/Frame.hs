
module Math.SL.Frame where

import qualified Data.Set as S

data Frame a = Frame (S.Set a)                  -- A simple frame.
             | FocusedFrame (S.Set a) (S.Set a) -- A frame partitioned into x, ~x
             deriving (Eq, Ord)

frame :: Ord a => [a] -> Frame a
frame = Frame . S.fromList

uncoarsen :: Ord a => Frame a -> Frame a
uncoarsen frm@(Frame _) = frm
uncoarsen (FocusedFrame xs ys) = Frame (xs `S.union` ys)

coarsen :: Ord a => Frame a -> Frame a -> Maybe (Frame a)
coarsen (FocusedFrame _ _) _ = Nothing
coarsen theta@(Frame set) x
  | x `isSubframeOf` theta = Just (FocusedFrame set' (set `S.difference` set'))
  | otherwise              = Nothing
  where
    (Frame set') = uncoarsen x

isBinomial :: Frame a -> Bool
isBinomial (FocusedFrame _ _) = True
isBinomial (Frame set) = hasLength (S.toList set) 2

isSubframeOf :: Ord a => Frame a -> Frame a -> Bool
isSubframeOf (Frame xs) (Frame ys) = xs `S.isSubsetOf` ys
isSubframeOf frm1 frm2             = uncoarsen frm1 `isSubframeOf` uncoarsen frm2

hasLength :: [a] -> Int -> Bool
hasLength [] 0     = True
hasLength [] n     = False
hasLength (_:_) 0  = False
hasLength (_:xs) n = hasLength xs (n - 1)
