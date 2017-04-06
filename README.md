Welcome to Haskov!
====================

Haskov is a Haskell library for *finite regular Markov chains*.

A *Markov* can be created from lists, maps, or by individual insertions. 

Haskov is licensed under the terms of the **MIT License**.

---------------

Requirements
-------------
Haskov relies on the [hmatrix](https://hackage.haskell.org/package/hmatrix "hmatrix") package. Please see [its installation page](https://github.com/albertoruiz/hmatrix/blob/master/INSTALL.md) for help.

-----------------

Getting started
------------------

Clone or download Haskov and move the source files to your project folder. Then import Haskov:
```haskell
import MyDirectory.Haskov (Markov)
import qualified MyDirectory.Haskov as Has
```

#### Markov Type
```Haskell
data Markov a
type IndexMap a = Map a Int
type HMatrixMap = Map ((Int, Int), Double)
```
A Markov chain of states *a* and the probabilities of moving from one state to another. It is made up of an IndexMap, mapping state *a* to its corresponding index in the HMatrixMap, which will be used for computations.

#### Construction

```Haskell
empty :: Markov a
``` 
Creates an empty Markov.

```Haskell
>>> empty
[]
```

```Haskell
insert :: (Ord a) => a -> a -> Double -> Markov a -> Markov a
```

Insert a pair of states (a, a) and their transition probability into a Markov.

```Haskell 
>>> insert "A" "B" 1.0 empty
[(("A", "B"), 1.0)]
```

#### From Lists
Markovs can also be created from a list.

```Haskell
fromList :: (Ord a) => [((a, a), Double)] -> Markov a
```

Creates a Markov from a list of state and transition probability tuples.

```Haskell
>>> fromList [(("A", "B"), 0.3), (("A", "A"), 0.7), (("B", "A"), 1.0)]
[(("A", "B"), 0.3), (("A", "A"), 0.7), (("B", "A"), 1.0)]
```

#### From Maps
Markovs can also be created from a map.


```Haskell
fromMap :: (Ord a) => Map (a, a) Double -> Markov a
```

Creates a Markov from a map of states and transition probabilities

```Haskell
>>> let m = Data.Map.fromList [(("A", "B"), 0.3), (("A", "A"), 0.7), (("B", "A"), 1.0)]
>>> fromMap m
[(("A", "B"), 0.3), (("A", "A"), 0.7), (("B", "A"), 1.0)]
```

#### Markov Chains
There are a few functions to manipulate and test Markov chains

```Haskell
walk :: (Ord a) => Markov a -> Int -> IO [a]
```

The walk function starts at the steady state of a Markov chain and takes *n* steps through the chain before stopping, returning a list of the steps taken. If an end state is reached before the number of specified steps, it halts.

```Haskell
steadyState :: (Ord a) =>  Markov a -> [(a, Double)]
```

The steady state (equilibrium distribution) of the Markov chain

#### HaskovText

HaskovText contains functions for converting text strings to Markov chains using Haskov. 

> Written with [StackEdit](https://stackedit.io/).