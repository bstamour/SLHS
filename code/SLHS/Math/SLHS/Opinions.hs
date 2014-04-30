

module Math.SLHS.Opinions where


import Math.SLHS.Base


data Binomial a =
  Binomial { binBelief      :: Rational
           , binDisbelief   :: Rational
           , binUncertainty :: Rational
           , binBaseRate    :: Rational
           , binX           :: Frame a
           , binNotX        :: Frame a
           }


class ToBinomial op where
  toBinomial :: op a -> Binomial a


data Multinomial a = Multinomial a


class ToMultinomial op where
  toMultinomial :: op a -> Multinomial a


data Hyper a = Hyper a


class ToHyper op where
  toHyper :: op a -> Hyper a
