# QuickCheck

Testing is an essential part of any project, so in this post we will look at
state of the art property testing library QuickCheck, which was originally
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
LiquidHaskell, that can help you with that. Proving correctness is not always
possible, some properties are either too hard or impossible to prove, moreover,
often we just want to check that they work on some inputs. One of the ways to do
that is through writing some unit tests, but since it is not feasable to test
all type of inputs exhaustively for most functions, we usuall check some corner
cases and possibly some other arbitrary values. Systematic generation of
arbitrary input could be very helpful in that scenario, and that's were
QuickCheck comes into play.

```haskell
import Test.QuickCheck

prop_RevRev :: Eq a => [a] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs
```

We can load those properties into GHCi and run `quickCheck` on them:

```
λ> quickCheck prop_RevRev
+++ OK, passed 100 tests.
λ> quickCheck prop_RevApp
+++ OK, passed 100 tests.
```

What just happened? QuickCheck called `prop_RevRev` and `prop_RevApp` 100 times
each, with random lists as arguments and declared those tests as passing,
because all calls resulted in `True`. Worth noting, that in reality, not only
`prop_RevRev`, but both of those properties are polymorphic and `quickCheck`
will be happy to work with such functions even if type signatures were inferred
and it will run just fine in GHCi. On the other hand, while writing a test
suite, we have to restrict the type signature to concrete types, such as `[Int]`
or `Char`, otherwise typechecker will get confused. For example, this program
will not compile:

```haskell
main :: IO ()
main = quickCheck (const True)
```

Now this is great, but how did we just passed two functions with different
number of arguments of different types to `quickCheck`, and how did it know what
to do with them? Let's look at it's type signature:

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

Here is another very simple property of lists `xs !! n == head (drop n xs)`,
so let's define it as is:

```haskell
prop_Index_v1 :: [Integer] -> Int -> Bool
prop_Index_v1 xs n = xs !! n == head (drop n xs)
```

Naturally, you can see a problem with that function, it cannot accept just any
random `Int` to be used for indexing, and `quickCheck` quickly finds that
problem for us and prints out violating input along with an error:

```haskell
λ> quickCheck prop_Index_v1
*** Failed! Exception: 'Prelude.!!: index too large' (after 1 test): 
[]
0
```

Interestingly, if you try to run this example on any computer, there is a very
good chance that it will give the same output, so it seems, that input to
properties is not completely random. In fact, thanks to the function `sized`,
the first input to our property will always be an empty list and an integer `0`,
which tend to be really good corner cases to test for. In our case, though, `!!`
and `head` are undefined for empty lists and negative numbers. We could add some
guards, but there are facilities provided for such common cases:

```haskell
prop_Index_v2 :: (NonEmptyList Integer) -> NonNegative Int -> Bool
prop_Index_v2 (NonEmpty xs) (NonNegative n) = xs !! n == head (drop n xs)
```

This version is still not quite right, since we do have another precodition
`n < length xs`. However, it would be a bit complicated to describe this relation 
through the type system, so we will specify this precondition at a runtime using
implication operator (⇒). Note, that return type has changed too:

```haskell
prop_Index_v3 :: (NonEmptyList Integer) -> NonNegative Int -> Property
prop_Index_v3 (NonEmpty xs) (NonNegative n) =
  n < length xs ==> xs !! n == head (drop n xs)
```

An alternative way to achieve the same affect would be to generate a valid index
within a property itself:

```haskell
prop_Index_v4 :: (NonEmptyList Integer) -> Property
prop_Index_v4 (NonEmpty xs) =
  forAll (choose (0, length xs-1)) $ \n -> xs !! n == head (drop n xs)
```

```
λ> quickCheck prop_Index_v3 >> quickCheck prop_Index_v4
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
```

Just in case, let's quickly dissect this for all (∀) business. It takes a random
value generator, which `choose` happens to produce, a property that operates on it's
values and returns a property, i.e. applies the values from a specific generator
to the supplied property.

```haskell
λ> :t forAll
forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
λ> sample' $ choose (0, 3)
[0,2,2,3,3,3,0,1,0,1,3]
```

There is a very subtle difference between the last two versions, namely `_v3`
will discard tests that do not satisfy a precondition, while `_v4` will always
generate a value for `n` that is safe for passing to index function. This is not
important for this example, which is good, but that is not always the
case. Whenever precondition is too strict, QuickCheck might give up early
looking for valid values for a test, but more importantly, it can give a false
sence of validity, since most of the values it will find will be trivial
ones.

For instance, considering that every prime number larger than 2 is odd, we can
easily derive a property that sum of any two prime numbers greater than 2 is
even. Instead of writing inefficient routines for prime numbers we will
use [primes](https://www.stackage.org/lts-7.16/package/primes-0.2.1.0)
package. Here is a naive way to test that property:

```haskell
prop_PrimeSum_v1 :: Int -> Int -> Property
prop_PrimeSum_v1 p q =
  p > 2 && q > 2 && isPrime p && isPrime q ==> even (p + q)
```

As you can imagine it is not too oftent that a random number will be prime, thus
affecting the quality of a test:

```haskell
λ> quickCheck prop_PrimeSum_v1
*** Gave up! Passed only 26 tests.
```

We can see that it only found 26 satisfiable tests, but how bad it actually is?
An quick way to check the values accepted by a test is to classify them somehow:

```haskell
prop_PrimeSum_v1' :: Int -> Int -> Property
prop_PrimeSum_v1' p q =
  p > 2 && q > 2 && isPrime p && isPrime q ==>
  classify (p < 20 && q < 20) "trivial" $ even (p + q)
```

```haskell
λ> quickCheck prop_PrimeSum_v1'
*** Gave up! Passed only 29 tests (96% trivial).
λ> quickCheckWith stdArgs { maxSuccess = 500 } prop_PrimeSum_v1'
*** Gave up! Passed only 94 tests (44% trivial).
```

We can see that the values this property was tested on are almost all trivial
ones. Increasing number of tests was not much help. This is due to the fact,
that by default, values generated for integers are pretty small, we could try to
fix that, but this time we will also generate a histogram of unique pairs of
discovered prime numbers:

```haskell
prop_PrimeSum_v2 :: (Positive (Large Int)) -> (Positive (Large Int)) -> Property
prop_PrimeSum_v2 (Positive (Large p)) (Positive (Large q)) =
  p > 2 && q > 2 && isPrime p && isPrime q ==>
  collect (if p < q then (p, q) else (q, p)) $ even (p + q)
```

```haskell
λ> quickCheck prop_PrimeSum_v2
*** Gave up! Passed only 24 tests:
16% (3,3)
 8% (11,41)
 4% (9413,24019)
 4% (93479,129917)
 ...
```

This is better, there are less trivial values, but still, number of tests is far
from satisfactory. It is also extremely inefficient to look for prime values
that way, and for any really large value it will take forever to check
its primality, it is much better to select from known prime values:

```haskell
prop_PrimeSum_v3 :: Property
prop_PrimeSum_v3 =
  forAll (choose (1, 1000)) $ \ i ->
    forAll (choose (1, 1000)) $ \ j ->
      let (p, q) = (primes !! i, primes !! j) in
      collect (if p < q then (p, q) else (q, p)) $ even (p + q)
```

```haskell
λ> quickCheck prop_PrimeSum_v3
+++ OK, passed 100 tests:
 1% (983,6473)
 1% (953,5059)
 1% (911,5471)
 ...
```

## Arbitrary


If for some reason we needed prime values for many tests, it would be a burden
to generate them this way for each property. Solution is to write an instance
for `Arbitrary`:

```haskell
newtype Prime a = Prime a deriving Show

instance (Integral a, Arbitrary a) => Arbitrary (Prime a) where
  arbitrary = do
    x <- frequency [ (10, choose (0, 1000))
                   , (5, choose (1001, 10000))
                   , (1, choose (10001, 50000))
                   ]
    return $ Prime (primes !! x)
```

Calculating large prime numbers is pretty expensive, so we could simply use
`choose (0, 1000)`, similarly to how it was done in `prop_PrimeSum_v3`, but
there is no reason why we should exclude generating random large prime numbers
completely, instead we can specify a custom distribution by using `frequency`.

Now writing `prop_PrimeSum` is a peice of cake:

```haskell
prop_PrimeSum :: Prime Int -> Prime Int -> Property
prop_PrimeSum (Prime p) (Prime q) =
  p > 2 && q > 2 ==> classify (p < 1000 || q < 1000) "has small prime" $ even (p + q)
```

```haskell
λ> quickCheck prop_PrimeSum
+++ OK, passed 100 tests (21% has small prime).
```

## CoArbitrary


There are quite a few instances of `Arbitrary`, many common data types from
`base` are, but the most peculiar one is a function:

```
λ> :i Arbitrary
class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]
...
instance [safe] (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)
...
```

That's right, QuickCheck can even generate functions for us! One restriction is
that an argument to the function is an instance of `CoArbitrary`, which also
means functions of any arity can be generated. Another caviat is that we need an
instance of `Show` for functions, which is not a standart practice, but
sometimes is anavoidable.


```haskell
instance Show (Int -> Char) where
  show _ = "Function: (Int -> Char)"

instance Show (Char -> Maybe Double) where
  show _ = "Function: (Char -> Maybe Double)"

prop_MapMap :: (Int -> Char) -> (Char -> Maybe Double) -> [Int] -> Bool
prop_MapMap f g ls = map g (map f ls) == map (g . f) ls
```


## HSpec


## Bootstrap example
