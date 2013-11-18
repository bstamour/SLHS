{-# LANGUAGE TypeFamilies #-}


module Logic 
       ( Logic(..)
       , (<>)
       ) where


import Data.Semigroup (Semigroup, (<>))


class Logic l where
  data Opinion l :: *
  combine :: Opinion l -> Opinion l -> Opinion l


instance Logic l => Semigroup (Opinion l) where
  (<>) = combine

