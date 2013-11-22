module Tests where


import Reasoner
import Logic
import SL

import Control.Applicative
import Data.Ratio ((%))
import qualified Data.Map as M


data Atoms = Red | Blue deriving (Show, Eq, Ord)


type MyReasoner a = Reasoner SL Atoms a


mass = makeMassAssignment [ ([Red],       1%4)
                          , ([Blue],      1%4)
                          , ([Red, Blue], 1%2)
                          ]


runTest r = flip run mass r


test0 = runTest $ map fst . M.toList . unMass <$> getMass


test1 = flip runSL mass f
  where
    f :: Reasoner SL Atoms [[Atoms]] 
    f = map fst . M.toList . unMass <$> getMass


test2 = runTest $ (+) <$> massOf [Red] <*> massOf [Blue]


test3 = runTest $ mapM massOf [[Red], [Blue], [Red, Blue]]


test4 = runTest $ do massr <- massOf [Red]
                     massb <- massOf [Blue]
                     return $ massr + massb


