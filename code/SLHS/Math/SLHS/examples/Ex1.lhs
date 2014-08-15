\documentclass[thesis.tex]{subfiles}

\begin{document}



\begin{code}
module Ex1 where

-- Import the required modules.
import Math.SLHS

-- Create the model data.
holders = ["Bob", "Bryan", "Richard"]
frame   = [1,2,3,4,5]

-- Make the initial state.
initial = makeState holders [frame] undefined

-- Build some expressions.
expr1 = getBinomial "Bob" 0
expr2 = getBinomial "Bryan" 0
expr3 = (expr1 `coarsenBy` (< 2)) +! (expr1 `coarsenBy` (< 4))
expr4 = expr3 `times` expr2

-- Compute the result.
result = initial >>= run expr4
\end{code}



\end{document}
