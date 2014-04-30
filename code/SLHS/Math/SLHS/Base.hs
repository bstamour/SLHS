


module Math.SLHS.Base
       ( Frame()
       , BeliefVector()
       ) where


import qualified Data.Set as S
import qualified Data.Map as M


newtype Frame a = Frame (S.Set a)


newtype BeliefVector a = BeliefVector (M.Map (Frame a) Rational)
