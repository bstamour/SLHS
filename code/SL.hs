{-# LANGUAGE TypeFamilies #-}


module SL 
       ( SL(..)
       , Opinion(..)
       , runSL
       , fusion
       , discount
       , (<&&>)
       , (<||>)
       ) where


import Reasoner
import Logic


data SL = SL


instance Logic SL where
  data Opinion SL = SLOpinion { stuff :: Int, more :: Int } deriving (Show, Eq)
  combine = fusion


runSL :: Reasoner SL atoms b -> MassAssignment atoms -> b
runSL = run


fusion :: Opinion SL -> Opinion SL -> Opinion SL
fusion = undefined


discount :: Opinion SL -> Opinion SL -> Opinion SL
discount = undefined


(<&&>) :: Opinion SL -> Opinion SL -> Opinion SL
op1 <&&> op2 = undefined


(<||>) :: Opinion SL -> Opinion SL -> Opinion SL
op1 <||> op2 = undefined
