module Math.SL.Frame where


import Math.SL.SLValue
import qualified Data.Set as S


data Frame a = Frame (S.Set a)                  -- A simple frame.
             | FocusedFrame (S.Set a) (S.Set a) -- A frame partitioned into x, ~x
             deriving (Eq, Ord, Show)


-- | Smart constructor. Build a frame from a list of items.
frame :: Ord a => [a] -> Frame a
frame = Frame . S.fromList


-- | Given a simple frame, coarsen it into a binomial frame.
coarsen :: Ord a => Frame a -> Frame a -> SLValue (Frame a)
coarsen (FocusedFrame _ _) _ = SLError "Can not coarsen a coarsened frame."
coarsen theta@(Frame set) x
  | x `isSubframeOf` theta = return (FocusedFrame set' (set `S.difference` set'))
  | otherwise              = SLError "x must be a subset of theta."
  where
    (Frame set') = uncoarsen x


-- | Reconstruct a simple frame from a partitioned binomial frame.
uncoarsen :: Ord a => Frame a -> Frame a
uncoarsen frm@(Frame _)        = frm
uncoarsen (FocusedFrame xs ys) = Frame (xs `S.union` ys)


isBinomial (FocusedFrame _ _) = True
isBinomial (Frame set)        = hasLength (S.toList set) 2


-- | Is one frame a subframe of another?
isSubframeOf :: Ord a => Frame a -> Frame a -> Bool
isSubframeOf (Frame xs) (Frame ys) = xs `S.isSubsetOf` ys
isSubframeOf frm1 frm2             = uncoarsen frm1 `isSubframeOf` uncoarsen frm2


hasLength :: [a] -> Int -> Bool
hasLength [] 0     = True
hasLength [] n     = False
hasLength (_:_) 0  = False
hasLength (_:xs) n = hasLength xs (n - 1)
