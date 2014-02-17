
module Math.SL
       ( SLValue(..)     -- Consider which ones to export. We don't want people making
       , SLState(..)     -- objects without going through the smart constructors...
       , SL
       , Opinion(..)
       , runSL
       , opinion
       , (<++>)
       , (</\>)
       ) where


import Math.SL.Core
import Math.SL.Opinion
import Math.SL.Operators
