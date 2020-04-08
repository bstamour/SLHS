\documentclass[thesis.tex]{subfiles}

\begin{document}

\ignore{
\begin{code}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Ex2 where

import Math.SLHS
import Data.Ratio
\end{code}
}

\subsection{Observing Genetic Mutations}

This example also comes from the draft Subjective Logic book. Assume through
a process of genetic engineering that we can produce two kinds of chicken eggs:
male, or female. Each egg, regardless of gender, can also have genetic mutation
S or T. The first sensor determines whether an egg is male or female, and the
second sensor measures whether the egg has genetic mutation S or T. This
scenario can be modeled by using two frames of discernment

\begin{code}
type Gender = Int
type Mutation = Int

m = 0
f = 1
s = 2
t = 3

gender = [m, f]
mutation = [s, t]
\end{code}

and two belief holders

\begin{code}
data Sensor = A | B deriving (Eq, Ord, Show)
sensors = [A, B]
\end{code}

Due to a limitation of SLHS, we must use the same underlying type for all
frames, hence we use integers to represent both genders and mutations.

Since the two sensors measure orthogonal aspects of the eggs, we can combine
their observations through multinomial multiplication to produce an opinion
over the cartesian product of the two frames. Assume we have two observations:

\begin{code}
obs1 = [(gender,   [(A, [([m], 8%10), ([f], 1%10)])])]
obs2 = [(mutation, [(B, [([s], 7%10), ([t], 1%10)])])]
observations = obs1 ++ obs2
\end{code}

with the following base rates:

\begin{code}
baseRates = [ (gender,   [(A, [(m, 1%2), (f, 1%2)])])
            , (mutation, [(B, [(s, 1%5), (t, 4%5)])])
            ]
\end{code}

We can then compute the opinion over the cartesian product by evaluating
the following expression:

\begin{code}
expression = getMultinomial A 0 `times` getMultinomial B 1
state      = makeState sensors [gender, mutation] observations baseRates
opinion    = state >>= run' expression
\end{code}

We can see the resulting multinomial opinion by running the command
\emph{print opinion}, which displays the following:

\begin{spec}
Multinomial:
  Holder: Product (Holder A) (Holder B)
  Frame: {(0,2),(0,3),(1,2),(1,3)}
  Belief: <((0,2),37823 % 61000),((0,3),11297 % 61000),
            ((1,2),249 % 2440), ((1,3),39 % 12200)>
  Uncertainty: 273 % 3050
  Base Rate: <((0,2),1 % 10),((0,3),2 % 5),((1,2),1 % 10),((1,3),2 % 5)>
\end{spec}

The fractions are a little messy, but with a trusty pocket calculator we can
verify that the beliefs plus the uncertainty sums to 1. This result is in fact
displayed with slightly more accuracy than the result in Josang's draft book.

\end{document}
