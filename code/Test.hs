{-# LANGUAGE NoMonomorphismRestriction #-}

module Test where

import Reasoner
import SL
import Control.Applicative
import Data.Ratio
import qualified Data.Map as M

data Holders = Bryan | Bob  deriving (Eq, Ord, Show)
data MyFrame = Red   | Blue deriving (Eq, Ord, Show, Bounded, Enum)

mass :: MassAssignment Holders MyFrame
mass = MassAssignment $ M.fromList
       [ (Bryan, MassMap $ M.fromList
                 [ (Theta,         1%3)
                 , (Subset [Red],  1%3)
                 , (Subset [Blue], 1%3)
                 ])
       , (Bob, MassMap $ M.fromList
               [ (Theta,              1%4)
               , (Subset [Red, Blue], 1%4)
               , (Subset [Blue],      1%4)
               , (Subset [Red],       1%4)
               ])
       ]

baseRate :: BaseRate MyFrame
baseRate = BaseRate $ M.fromList
           [ (Red,  1%2)
           , (Blue, 1%2)
           ]

op1 = binomial Bryan [Red]
op2 = binomial Bryan [Blue]
expr = binBelief <$> binomialSum op1 op2
value = run expr mass baseRate

main :: IO ()
main = putStrLn "Hello"