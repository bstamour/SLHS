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
S or T. The first sensor determines whether an egg ois male or female, and the
second sensor measures whether the egg has genetic mutation S or T. This
scenario can be modelled by using two frames of discernment

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
data Sensor = A | B deriving (Eq, Ord)
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
opinion    = state >>= run expression
\end{code}


\end{document}
