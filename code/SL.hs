



module SL
       ( SLValue(..)     -- Consider which ones to export. We don't want people making
       , SLState(..)     -- objects without going through the smart constructors...
       , SL
       , Opinion(..)
       , runSL
       , opinion
       , (<++>)
       , (</\>)
       ) where




import Core
import Opinion
import Operators
