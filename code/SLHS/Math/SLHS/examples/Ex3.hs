{-# LANGUAGE NoMonomorphismRestriction #-}

module Ex3 where

import Math.SLHS
import Math.SLHS.Extensions
import qualified Math.SLHS.Frame as F
import Data.Ratio

temperatures = [1.. 100] -- freezing to boiling.

obs = [(temperatures, [ (0, [([44], 1%10), ([45], 8%10), ([46], 1%10)] )
                      , (1, [([25], 5%10), ([46], 4%10) ] )
                      ])]

baseRates = [ (temperatures, [ (0, zip temperatures (repeat (1%100)))
                             , (1, zip temperatures (repeat (1%100)))
                             ])]

cold = F.fromList [0.. 20]
warm = F.fromList [21.. 35]
hot  = F.fromList [36.. 100]

expr1 = getHyper 0 0 `aFuse` getHyper 1 0

expr3 = do e1 <- coarsen expr1 cold
           let e2 = uncoarsen e1
           return e2

state = makeState [0, 1] [temperatures] obs baseRates
res = state >>= run' expr3

expr2 = do e1 <- expr1
           let op = hyperCoarsen e1 [ cold, warm, hot ]
           return op

result = state >>= run' expr2
