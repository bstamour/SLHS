

module Math.SLHS.Opinions where


data Binomial a = Binomial a


class ToBinomial op where
  toBinomial :: op a -> Binomial a


data Multinomial a = Multinomial a


class ToMultinomial op where
  toMultinomial :: op a -> Multinomial a


data Hyper a = Hyper a


class ToHyper op where
  toHyper :: op a -> Hyper a
