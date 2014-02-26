module Math.SL.Frame where

import qualified Data.Set as S


newtype Frame a = Frame (S.Set a) deriving (Eq, Show)
