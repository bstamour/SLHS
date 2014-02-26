module Math.SL.Opinion where

import Math.SL.Core

import Control.Applicative


data OpType = Binomial | Multinomial | Hyper deriving (Eq, Show, Ord)


data Opinion a = Opinion
                 OpType
                 (BeliefVector a)
                 Rational
                 (BaseRateVector a)
                 deriving (Eq, Show)


data Coarsened a = X (Frame a) | NotX (Frame a) deriving (Show, Eq, Ord)


--------------------------------------------------------------------------------------------
-- Constructors.
--------------------------------------------------------------------------------------------


makeOpinion :: Ord a
               =>Frame a
               -> BeliefVector a
               -> BaseRateVector a
               -> SLValue (Opinion a)
makeOpinion theta b a
  | not (b `bvIsDefinedOver` theta)  = SLError "error"
  | not (a `brvIsDefinedOver` theta) = SLError "error"
  | otherwise = pure $ Opinion optype b u a
  where
    u      = bvUncertainty b
    optype = computeOpType b

    computeOpType :: BeliefVector a -> OpType
    computeOpType b | notClass1 = Hyper
                    | notBinary = Multinomial
                    | otherwise = Binomial
      where
        notClass1 = any (\f -> cardinality f /= 1) focals
        notBinary = length focals /= 2
        focals    = bvFocalElements b


makeCoarsened :: Ord a
                 => Frame a
                 -> BeliefVector a
                 -> BaseRateVector a
                 -> Frame a
                 -> SLValue (Opinion (Coarsened a))
makeCoarsened theta b a x
  | not (b `bvIsDefinedOver` theta)  = SLError "error"
  | not (a `brvIsDefinedOver` theta) = SLError "error"
  | not (x `isSubframeOf` theta)     = SLError "error"
  | otherwise = Opinion <$> pure Binomial <*> b' <*> pure u <*> a'
  where
    y      = theta `difference` x
    bel    = sum . mapFrame (beliefOf b . frame . pure) $ x
    dis    = sum . mapFrame (beliefOf b . frame . pure) $ y
    atom   = sum . mapFrame (atomicityOf a) $ x
    u      = 1 - bel - dis
    theta' = frame [X x, NotX y]
    b'     = beliefVector   theta' [(frame [X x], bel), (frame [NotX y], dis)]
    a'     = baseRateVector theta' [(X x, atom), (NotX y, 1 - atom)]


--------------------------------------------------------------------------------------------
-- Querying functions.
--------------------------------------------------------------------------------------------


isHyper :: Opinion a -> Bool
isHyper _ = True

isMultinomial :: Opinion a -> Bool
isMultinomial (Opinion Hyper _ _ _) = False
isMultinomial _                     = True

isBinomial :: Opinion a -> Bool
isBinomial (Opinion Binomial _ _ _) = True
isBinomial _                        = False
