# Containers

In order to solve almost any problem, that requires manipulation of data, the
very first question should be: What is the most suitable container that we can
use for the problem at hand?

## Preface

Haskell is a pure functional language, thus, by it's nature, most of the
available containers are immutable and, without a doubt, the most common one is
a list `[a]`. Certainly, it is not efficiency that made list so popular, but
rather its simplicity, consequently it is also the first type of container that
you get introduced to while learning Haskell. Although, lists are perfectly
suatable for some problems, more often than not, we need something that is more
tailored to how we are trying to use our data.

Here are some situations
that [containers](https://www.stackage.org/lts/package/containers) package can
be of help. It contains efficient implementation of some of the most commonly
used containers used in programming:
* `Data.Set` - you care about uniqueness and possibly the order of elements.
* `Data.Map` - you need a mapping from unique keys to values and operations you
  perform can take advantage of ordering of keys.
* `Data.IntSet` and `Data.IntMap` - just as above, but when elements and keys
  respectfully are `Int`s.
* `Data.Sequence` - can be of use when a linear structure is required for a
  finite number of elements with fast access from its both sides and a fast
  concatenation with other sequences.
* `Data.Tree` and `Data.Graph` - for describing more complicated relations of
  elements.

## Map

`Map` is one of the most interesting and commonly used abstractions from the
package, so most of the examples will be based on it. Moreover, interface
provided for `Map`, `IntMap`, `Set` and `IntSet` is very similar, so
corresponding examples can be easily derived for all of the above.

### Setup a problem.

One of the common mappings in real life that we encounter is a person's
identification number, that maps a unique number to an actual human
being. Social Security Number (SSN) is normally used for that purpose in the
USA. Although it is a 9 digit number and using `IntMap` would be more efficient,
it does have some structure to it and we will take advantage of it, so we will
use a custom data type `SSN` instead of an `Int` as a key.

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-7.7 runghc --package containers
import qualified Data.Map as Map
import Data.List as List
import Text.Printf

data SSN = SSN
  { ssnPrefix :: Int
  , ssnInfix :: Int
  , ssnSuffix :: Int
  } deriving (Eq, Ord)

instance Show SSN where
  show (SSN p i s) = List.intercalate "-" [printf "%03d" p, printf "%02d" i, printf "%03d" s]
  
data Person = Person
  { firstName :: String
  , lastName :: String
  } deriving (Eq, Show)

instance Show Person where
  show (Person fName lName) = fName ++ ' ':lName
```

While `SSN` will be used as a key in our `Map` to a `Person`, I would like to
stress how important `Eq` and `Ord` instances of a key actually are. Because
they are used for underlying representation of a `Map`, providing incomplete or
incorrect instances for these classes will lead to some strange behavior of your
data mappings, therefore either make sure you know what you are doing when creating
custom instances, or used derived instances, as they are always safe.

Because Social Security Numbers have a specific structure we would like to make sure
they are formatted properly by providing a constructor function that does some
validation.

```haskell
ssn :: Int -> Int -> Int -> SSN
ssn p i s
  | p <= 0 || p > 999 = error $ "Invalid SSN prefix: " ++ show p
  | i < 0 || i > 99 = error $ "Invalid SSN infix: " ++ show i
  | s < 0 || s > 9999 = error $ "Invalid SSN suffix: " ++ show s
  | otherwise = SSN p i s
```


Moreover, until a few years ago, ssn prefixes could tell you the
actual state the number was issued in, which for the sake of example we will
pretend still is the case and abuse that pattern.

### Converting maps

Let's go ahead and create our mapping of employees using `Map.fromList`:

```haskell
type Employees = Map.Map SSN Person

employees :: Employees
employees =
  Map.fromList
    [ (ssn 525 21 5423, Person "John" "Doe")
    , (ssn 521 01 8756, Person "Mary" "Jones")
    , (ssn 585 11 1234, Person "William" "Smith")
    , (ssn 525 15 5673, Person "Maria" "Gonzalez")
    , (ssn 523 34 1234, Person "Bob" "Jones")
    , (ssn 522 43 9862, Person "John" "Doe")
    , (ssn 527 75 1035, Person "Julia" "Bloom")
    ]
```

As you can see above, there is no particular order in our data as we defined
it, which results in creation of a `Map` in _O(n*log n)_ time complexity, but if
we were sure ahead of time that our list was sorted and unique with respect to
the first element of the tuple, it would be more efficient to use
`Map.fromAsclist`, which would run in _O(n)_ complexity instead.

### Operate on data.

Now that we have our program properly set up, most of available functions will
correspond to functions that we will try to use on our data, e.g. `lookupEmployee`:

```haskell
lookupEmployee :: SSN -> Employees -> Maybe Person
lookupEmployee = Map.lookup
```

which does exactly what is expected of it:

```haskell
λ> lookupEmployee (ssn 523 34 1234) employees
Just (Person {firstName = "Marry", lastName = "Jones"})
λ> lookupEmployee (ssn 555 12 3456) employees
Nothing
```

In order to refrain from redefining functions which trivially correspond to exisitng
ones, let's go through some of them:

* Getting total number of employees.
```haskell
λ> Map.size employees
7
```
* Checking existence of an employee by the social security number:
```haskell
λ> Map.member (ssn 585 11 1234) employees
True
λ> Map.member (ssn 621 24 8736) employees
False
```
* Looking up an employee with a default name:
```haskell
λ> Map.findWithDefault (Person "Bill" "Smith") (ssn 585 11 1234) employees
Person {firstName = "William", lastName = "Smith"}
λ> Map.findWithDefault (Person "Anthony" "Richardson") (ssn 621 24 8736) employees
Person {firstName = "Anthony", lastName = "Richardson"}
```
* Deleting an employee:
```haskell
λ> Map.size $ Map.delete (ssn 585 11 1234) employees
6
λ> Map.size $ Map.delete (ssn 621 24 8736) employees
7
```
* Adding an employee:
```haskell
λ> Map.size $ Map.insert (ssn 621 24 8736) (Person "Anthony" "Richardson") employees
8
```

### Folding

It would be useful to present our `Employees` in a user friendly format, so
let's define a `showMap` function by converting our `Map` to a printable string:

```haskell
showMap :: Employees -> String
showMap = List.intercalate "\n" . map showEmployee . Map.toList
```

Let's give it a try:

```haskell
λ> putStrLn $ showMap employees
(521-01-8756,Mary Jones)
(522-43-9862,John Doe)
(523-34-1234,Bob Jones)
(525-15-5673,Maria Gonzalez)
(525-21-5423,John Doe)
(527-75-1035,Julia Bloom)
(585-11-1234,William Smith)
```

Worth noting, that all emplyees are sorted by their SSN and conversion to a list
is done in ascending order, but in order to guarantee this behavior
`Map.toAscList` should be used instead or `Map.toDescList` to get it in a
reversed order. Conversion is nice an simple, but how about using folding in a
way that is native to `Map`? We should probably improve formatting as
well. Unfortunately, in order to get a similar output we need to handle a
special case of omitting addition of a new line character to the first entry in
the table, so we have to treat it separatly and handle a case when `Map` is
empty.

```haskell
showEmployee :: (SSN, Person) -> String
showEmployee (social, person) =
  concat [show social, ": ", show person]

showEmployees :: Employees -> String
showEmployees es
  | Map.null es = ""
  | otherwise = showE ssn0 person0 ++ Map.foldrWithKey prepender "" rest
  where
    showE = curry showEmployee
    ((ssn0, person0), rest) = Map.deleteFindMin es
    prepender key person acc = '\n' : showE key person ++ acc
```

Now that looks a bit nicer:
```haskell
λ> putStrLn $ showEmployees $ employees
521-01-8756: Mary Jones
522-43-9862: John Doe
523-34-1234: Bob Jones
525-15-5673: Maria Gonzalez
525-21-5423: John Doe
527-75-1035: Julia Bloom
585-11-1234: William Smith
```

__Excersise__: Using the fact that List is an instance `Monoid` implement
`showEmployees` with the help of `Map.foldMapWithKey`.

__Excersise__: Implement `showEmployeesReversed` using `Map.foldlWithKey`
(_hint_: use `Map.deleteFindMax`).


### Mapping

There is a set of mapping functions available range from simple `Map.map` to
more complex `Map.mapAccumRWithKey`. 

Because `Map` is an instance of `Functor` we can use `fmap` for mapping a
function over it's values, but for this example we will use it's equivalent
`Map.map` to convert values to `String` and `Map.elems` to retrieve a list of
new elements:

```haskell
λ> Map.elems $ Map.map show employees
["Mary Jones","John Doe","Bob Jones","Maria Gonzalez","John Doe","Julia Bloom","William Smith"]
```

If for some reson we would like to map a function over the keys we can use
`Map.mapKeys` for this:

```haskell:
λ> Map.keys $ Map.mapKeys show employees
["521-01-8756","522-43-9862","523-34-1234","525-15-5673","525-21-5423","527-75-1035","585-11-1234"]
```

We need to pay some extra attention to usage of `Map.mapKeys`, because whenever
the function that is being mapped over is not 1-to-1, it is possible that some
values will be lost. Although sometimes it may be desired to discard particular
elements, here is an example of how we can loose an employee if we assume that
last four numbers of a social are unique:

```haskell
λ> putStrLn $ showMap $ Map.mapKeys ssnSuffix employees
(1035,Julia Bloom)
(1234,William Smith)
(5423,John Doe)
(5673,Maria Gonzalez)
(8756,Mary Jones)
(9862,John Doe)
```

If we are sure that our function is not only 1-to-1, but it is also monotonic
(i.e. it doesn't change the order of the resulting keys) we could use a more
efficient mapping function `Map.mapKeysMonotonic`. Say a `show` function on
`SSN` would be safe to use, since ordering is preserved in our
implementation. One of the simplest examples of a non-monotonic function would be
`negate` and strictly-monotonic `succ`.
