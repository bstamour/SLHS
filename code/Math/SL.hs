module Math.SL
       ( opinion
       , frame
       , add
       , subtract
       , cFuse
       , aFuse
       , constrain
       , multiply
       , comultiply
       , divide
       , codivide
       , deduce
       , abduce
       , runSL
       , Holder(..)
       , SLData(..)
       ) where


import Prelude hiding (subtract)
import Control.Applicative

import Math.SL.State
import Math.SL.Core
import Math.SL.Frame
import Math.SL.SLValue
import Math.SL.Opinion
import Math.SL.Operators
