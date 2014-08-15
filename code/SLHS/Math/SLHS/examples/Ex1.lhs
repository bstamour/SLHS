\documentclass[thesis.tex]{subfiles}

\begin{document}



\begin{code}
module Ex1 where

-- Import the required modules.
import Math.SLHS
import Data.Ratio

-- Create the model data.
holders = ["Bob", "Bryan", "Richard"]
frame   = [1,2,3,4,5]

vectors =
  [ (frame, [ makeBeliefVector  "Bob"
              [(1, 1%5), (2, 1%5), (3, 1%5), (4, 1%5), (5, 1%5)]
            , makeBeliefVector  "Bryan"
              [(1, 1%4), (2, 1%2), (3, 1%6)]
            , makeBeliefVectorH "Richard"
              [([1,2,3], 1%2), ([4,5], 1%2)]
            ])
  ]

baseRate = [(1, 1%5), (2, 1%5), (3, 1%5), (4, 1%5), (5, 1%5)]

baseRates =
  [ (frame, [ makeBaseRateVector "Bob"     baseRate
            , makeBaseRateVector "Bryan"   baseRate
            , makeBaseRateVector "Richard" baseRate
            ])
  ]

-- Make the initial state.
initial = makeState holders [frame] vectors baseRates

-- Build some expressions.
expr1 = getMultinomial "Bob" 0
expr2 = getMultinomial "Bryan" 0
expr3 = (expr1 `coarsenBy` (< 2)) +! (expr1 `coarsenBy` (< 4))
expr4 = expr3 `times` expr2

-- Compute the result.
result = initial >>= run expr4
\end{code}



\end{document}
