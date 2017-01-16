# QuickCheck

Testing is an essential part of any project, so in this post we will look at
QuickCheck, state of the art property testing library, which was originally
invented in Haskell and later ported to many other languages.

## Properties

It seems that every tutorial on QuickCheck starts with the description of what a
function property is by listing properties of `reverse` function. Not to break
this tradition we'll do the same here:

```haskell
reverse [x] == [x]

reverse (reverse xs) == xs

reverse (xs ++ ys) == reverse ys ++ reverse xs
```

These properties hold for all finite lists with total values. Naturally, there
are ways to prove them and there are even tools for Haskell, such as
LiquidHaskell, that can help you with that. But this is not always possible,
some properties are either too hard or impossible to prove, moreover, often we
just want to verify that they work on some inputs. One of the ways to do that is
through writing some unit tests, that check some corner cases and possibly some
arbitrary lists, since it is not feasable to test all type of inputs
exhaustively. Systematic generation of arbitrary input could be very helpful in
that scenario, and that's were QuickCheck comes into play.

```haskell
import Test.QuickCheck

prop_RevRev :: Eq a => [a] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs
```

We can load that property into ghci and run `quickCheck` on it:

```
λ> quickCheck prop_RevRev
+++ OK, passed 100 tests.
λ> quickCheck prop_RevApp
+++ OK, passed 100 tests.
```

What just happened? QuickCheck called `prop_RevRev` and `prop_RevApp` 100 times
each, with random lists as arguments and declared them as passing tests, since
all calls resulted in `True`. Worth noting, that in reality, both of those
properties are polymorphic, and that `quickCheck` will be happy to work with
functions even with inferred type signatures, it will run just fine in GHCi, but
while writing a test suite, we have to restrict the type signature to concrete
types, such as `[Int]` or `Char`, otherwise typechecker will get confused. For
example, this program will not compile:
```haskell
main :: IO ()
main = quickCheck (const True)
```

Now this is great, but how did it do it? We just passed two functions with
different number of arguments of different types to `quickCheck`, how did it
know what to do? Let's look at it's type signature:

```haskell
λ> :t quickCheck
quickCheck :: Testable prop => prop -> IO ()
```

## Testable

So, it seems, that QuickCheck can test anything that is `Testable`:

```haskell
λ> :i Testable
class Testable prop where
  property :: prop -> Property
  exhaustive :: prop -> Bool
instance [safe] Testable Property
instance [safe] Testable prop => Testable (Gen prop)
instance [safe] Testable Discard
instance [safe] Testable Bool
instance [safe] (Arbitrary a, Show a, Testable prop) => Testable (a -> prop)
```

The last instance is for a function (`a -> prop`), that returns a `prop`, which,
in turn, must also be an instance of `Testable`. This magic trick of a recursive
constraint for an instance definition allows `quickCheck` to test a function
with any number of arguments, as long as each one of them is an instance of
`Arbitrary` and `Show`. So here is a check list of requirements for writing a
testable property:

* Zero or more aruments, that are an instance of `Arbitrary`, which is used for
  generating random input. More on that later.
* Arguments must also be an instance of `Show`, so if a test fails it can print
  the value, that actually triggered the failure.
* Return value is either:

  * `True`/`False` - to indicate pass/fail of a test case.
  * `Discard` - to skip the test case (eg. unimplemented test case).
  * `Result` - to customize pass/fail/discard test result behavior, collect
    extra information about the test outcome, provide callbacks and other
    advanced features.
  * `Property` for a much finer control of test logic. Such properties can be
    used as combinators to construct more complex test cases.
  * `Prop` used to implement `Property`
  
* Start with `prop_` or `prop`, followed by the usual `camelCase`, but
  that is just a convention, not a requirement.
* Has no side effects. Also not a requirement, but strongly suggested, since
  referential transparency is lost with `IO` monad and test results can be
  inconsistent between runs. At the same time there are capabilities for testing
  Monadic code, which we will not go into here.


## Preconditions

Here is another very simple property of lists `(xs !! n) == head (drop n xs)`,
so let's define it as is:

```haskell
prop_Index_v1 :: [Integer] -> Int -> Bool
prop_Index_v1 xs n = (xs !! n) == head (drop n xs)
```

Naturally, you can see a problem with that function, it cannot accept just any
random `Int` to be used for indexing, and `quickCheck` quickly finds a
counterexample for us.

```haskell
λ> quickCheck prop_Index_v1
*** Failed! Exception: 'Prelude.!!: index too large' (after 1 test): 
[]
0
```

Interestingly, if you try to run this example on any computer, it will give the
same output, so it seems, that input to properties is not completely random. In
fact, thanks to the function `sized`, the first input to our property
will always be an empty list and an integer `0`, which tend to be really good
corner cases to test for. In our case, though, `!!` and `head` are undefined for
empty lists, negative numbers. We could add some guards, but there are
facilities provided for such common cases:

```haskell
prop_Index_v2 :: (NonEmptyList Integer) -> NonNegative Int -> Bool
prop_Index_v2 (NonEmpty xs) (NonNegative n) = (xs !! n) == head (drop n xs)
```

This version is still not quite right, since we do have another precodition
`n < length xs`. However, it would be a bit complicated to describe this relation 
through the type system, so we will specify this precondition at a runtime using
implies `==>` function. Note, that return type has changed too:

```haskell
prop_Index_v3 :: [Integer] -> NonNegative Int -> Property
prop_Index_v3 xs (NonNegative n) = n < length xs ==> (xs !! n) == head (drop n xs)
```

```haskell
λ> quickCheck $ prop_Index_v3
+++ OK, passed 100 tests.
```




```haskell
prop_sqrt :: NonNegative Double -> Bool
prop_sqrt (NonNegative x)
  | x == 0 || x == 1 = sqrtX == x
  | x < 1 = sqrtX > 0 && sqrtX > x
  | x > 1 = sqrtX > 0 && sqrtX < x
  where
    sqrtX = sqrt x
```


## Arbitrary

There are quite a few instances of `Arbitrary`, many common data types from
`base` are, but the most interesting one is a function:

```
λ> :i Arbitrary
class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]
...
instance [safe] (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)
...
```


## CoArbitrary


## Purity


## HSpec


## Bootstrap example
