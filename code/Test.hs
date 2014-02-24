module Test where


import Prelude hiding (subtract)
import Control.Applicative
import Math.SL


h1 = Holder 1
h2 = Holder 2
h3 = Holder 3
h4 = Holder 4


-- Some single-frame operations.
e1 = opinion 0 h1 `add` opinion 0 h2
e2 = opinion 0 h1 `add` (opinion 0 h2 `subtract` opinion 0 h3)
e3 = (opinion 0 h1 `add` opinion 0 h2) `constrain` (opinion 0 h3 `add` opinion 0 h4)


-- Some multi-frame operations.
e4 = opinion 0 h1 `deduce` opinion 1 h2
e5 = opinion 0 h1 `codivide` opinion 1 h2
