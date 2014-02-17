module Test where

import Math.SL

e1 = opinion <++> opinion
e2 = opinion </\> (opinion <++> opinion)
e3 = (opinion </\> opinion) <++> (opinion </\> opinion)

runTest = flip runSL undefined