{-# LANGUAGE TypeFamilies #-}


module SL 
       ( SL(..)
       , Opinion(..)
       , runSL
       , fusion
       ) where


import Reasoner
import Logic


data SL = SL


instance Logic SL where
  data Opinion SL = SLOpinion { stuff :: Int, more :: Int } deriving (Show, Eq)
  combine = fusion


runSL :: Reasoner SL atoms b -> MassAssignment atoms -> b
runSL = runReasoner


fusion :: Opinion SL -> Opinion SL -> Opinion SL
fusion = undefined

