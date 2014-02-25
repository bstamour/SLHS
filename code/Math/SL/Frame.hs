module Math.SL.Frame where


import Math.SL.SLValue
import qualified Data.Set as S


data Frame a = Frame (S.Set a) deriving (Eq, Ord, Show)


-- | Smart constructor. Build a frame from a list of items.
frame :: Ord a => [a] -> Frame a
frame = Frame . S.fromList


--isBinomial :: Frame a -> Bool
--isBinomial (Frame set) = hasLength (S.toList set) 2


-- | Is one frame a subframe of another?
isSubframeOf :: Ord a => Frame a -> Frame a -> Bool
isSubframeOf (Frame xs) (Frame ys) = xs `S.isSubsetOf` ys


hasLength :: [a] -> Int -> Bool
hasLength [] 0     = True
hasLength [] n     = False
hasLength (_:_) 0  = False
hasLength (_:xs) n = hasLength xs (n - 1)
