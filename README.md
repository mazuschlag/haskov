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

###Markov Type
```Haskell
data Markov a
type IndexMap a = Map a Int
type HMatrixMap = Map ((Int, Int), Double)
```
A Markov chain of states *a* and the probabilities of moving from one state to another. It is made up of an IndexMap, mapping state *a* to its corresponding index in the HMatrixMap, which will be used for computations.

### Construction

##### Empty

```Haskell
empty :: Markov a
``` 
Creates an empty Markov.

```Haskell
>>> empty
[]
```

##### Insertion

```Haskell
insert :: (Ord a) => a -> a -> Double -> Markov a -> Markov a
```

Insert a pair of states (a, a) and their transition probability into a Markov. If the pair of states is already in  the Markov chain, the probability distribution is replaced.

```Haskell 
>>> insert "A" "B" 1.0 empty
[(("A", "B"), 1.0)]
```

##### Additional Insertions

```Haskell
insertWith :: (Ord a) => (Double -> Double -> Double) -> a -> a -> Double -> Markov a -> Markov a
```

Insert with a function on Doubles, combining new and old values. If the pair of states is not in the Markov chain, a normal insertion occurs. If the pair of states already exists, the probability distribution is updated by applying the function and input Double to the current probability distribution.

```Haskell
>>> let x = fromList [(("A", "B"), 0.3), (("A", "A"), 0.7), (("B", "A"), 1.0)]
>>> insertWith (+) "A" "C" 1.0 x
[(("B","A"),1.0),(("A","C"),1.0),(("A","B"),0.3),(("A","A"),0.7)]
>>> insertWith (+) "A" "B" 0.3 x
[(("B","A"),1.0),(("A","B"),0.6),(("A","A"),0.7)]
```

Note that it is highly suggested that you normalize after any insertion.

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

##### Walk

```Haskell
walk :: (Ord a) => Int -> Markov a -> IO [a]
```

The walk function starts at the steady state of a Markov chain and takes *n* steps through the chain before stopping, returning a list of the steps taken. If an end state is reached before the number of specified steps, it halts.

```Haskell
>>> let x = fromList [(("A", "B"), 0.3), (("A", "A"), 0.7), (("B", "A"), 1.0)]
>>> walk 5 x
["A","B","A","B","A"]
```
##### Steady State
```Haskell
steadyState :: (Ord a) =>  Markov a -> [(a, Double)]
```

The steady state (equilibrium distribution) of the Markov chain

```Haskell
>>> let x = fromList [(("A", "B"), 0.3), (("A", "A"), 0.7), (("B", "A"), 1.0)]
>>> steadyState x
[("A",0.7692307692307692),("B",0.23076923076923078)]
```

##### Normalize

```Haskell
normalize :: (Ord a) => Markov a -> Markov a
```

Normalizes the Markov chain. Note that this is necessary for many other chain functions to work properly (i.e. walk and steadyState)

```Haskell
>>> let x = fromList [(("A", "B"), 9), (("A", "A"), 21), (("B", "A"), 15)]
>>> normalize x
[(("B","A"),1.0),(("A","B"),0.3),(("A","A"),0.7)]
```

####HaskovText

HaskovText provides functions for converting text strings to Markov chains using Haskov. 

```Haskell
generate :: Text -> Markov Text
```
Creates a Haskov Markov chain from a Text.  See [Data.Text](https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html) 

```Haskell
>>> let text = pack "A A A A A B A A B A A B A"
>>> generate text 
[(("B","A"),3.0),(("A","B"),3.0),(("A","A"),7.0)]
>>> Haskov.normalize . generate $ text
[(("B","A"),1.0),(("A","B"),0.3),(("A","A"),0.7)]
```

> Written with [StackEdit](https://stackedit.io/).