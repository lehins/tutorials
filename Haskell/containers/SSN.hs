#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-7.7 runghc --package containers

import qualified Data.Map as Map
import Data.List as List
import Data.Monoid
import Text.Printf

-- | Reason why I am using custom data type for this example is to emphasize the
-- importance of the `Eq` and `Ord` instances of the Map's key.
--
-- Social Security Number. Commonly used as a unique identification number of a
-- person. <https://www.ssa.gov/employer/stateweb.htm>
data SSN = SSN
  { ssnPrefix :: Int
  , ssnInfix :: Int
  , ssnSuffix :: Int
  } deriving (Eq, Ord)

instance Show SSN where
  show (SSN p i s) = List.intercalate "-" [printf "%03d" p, printf "%02d" i, printf "%03d" s]

ssn :: Int -> Int -> Int -> SSN
ssn p i s
  | p <= 0 || p > 999 = error $ "Invalid SSN prefix: " ++ show p
  | i < 0 || i > 99 = error $ "Invalid SSN infix: " ++ show i
  | s < 0 || s > 9999 = error $ "Invalid SSN suffix: " ++ show s
  | otherwise = SSN p i s


data Person = Person
  { firstName :: String
  , lastName :: String
  } deriving (Eq)

instance Show Person where
  show (Person fName lName) = fName ++ ' ':lName

type Employees = Map.Map SSN Person




-- | Creation using fromList
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


showMap :: (Show k, Show v) => Map.Map k v -> String
showMap = List.intercalate "\n" . map show . Map.toList



showEmployee :: (SSN, Person) -> String
showEmployee (social, person) =
  concat [show social, ": ", show person]



showEmployeesBad :: Employees -> String
showEmployeesBad es
  | Map.null es = ""
  | otherwise = tail $ Map.foldrWithKey appender "" es
  where
    appender key person list = '\n' : (curry showEmployee) key person ++ list


showEmployees :: Employees -> String
showEmployees es
  | Map.null es = ""
  | otherwise = showE ssn0 person0 ++ Map.foldrWithKey prepender "" rest
  where
    showE = curry showEmployee
    ((ssn0, person0), rest) = Map.deleteFindMin es
    prepender key person acc = '\n' : showE key person ++ acc


-- |
-- λ> lookupEmployee (ssn 523 34 1234) employees
-- Just (Person {firstName = "Marry", lastName = "Jones"})
lookupEmployee :: SSN -> Employees -> Maybe Person
lookupEmployee = Map.lookup



-- Map.insert
-- | Adding an employee
-- λ> Map.insert (ssn 987 78 1323) (Person "Joanna" "Bloom") employees


-- | λ> putStrLn $ showEmployees $ Map.map keepFirstInitial employees
keepFirstInitial :: Person -> Person
keepFirstInitial p@(Person (x:_) _) = p { firstName = x:"." }

-- Map.map
conciseEmployees :: Employees
conciseEmployees = Map.map keepFirstInitial employees


-- Map.insertWith
-- putStrLn $ showEmployees $ Map.insertWith keepFirstInitial (ssn 987 78 1323) (Person "Joanna" "Bloom") $ Map.map firstInitial employees


-- | Got married, changed her last name, let's make sure first name hasn't
-- changed, but add her as a new employee if she doesn't exist in db yet.
-- λ> putStrLn $ showEmployees $ changedLastName (ssn 987 78 1323) (Person "Joanna" "Carter") employees
-- λ> putStrLn $ showEmployees $ changedLastName (ssn 527 75 1035) (Person "Julia" "Carter") employees
changedLastName :: SSN -> Person -> Employees -> Employees
changedLastName = Map.insertWith checkFirstName
  where
    checkFirstName p1 p2
      | firstName p1 /= firstName p2 = error "Fisrt name has changed."
      | otherwise = p1 -- keep attention: the first argument is the new element.



showEmployeesM :: Employees -> String
showEmployeesM es
  | Map.null es = mempty
  | otherwise = showE firstSSN firstName <> Map.foldMapWithKey prepender rest
  where
    showE = curry showEmployee
    ((firstSSN, firstName), rest) = Map.deleteFindMin es
    prepender key person = '\n' : showE key person


showEmployeesReversed :: Employees -> String
showEmployeesReversed es
  | Map.null es = ""
  | otherwise = showE firstSSN firstName <> Map.foldlWithKey prepender "" rest
  where
    showE = curry showEmployee
    ((firstSSN, firstName), rest) = Map.deleteFindMax es
    prepender acc key person = '\n' : showE key person ++ acc

{-
data Candidate = Candidate1
               | Candidate2
               | Candidate3

vote :: SSN -> Candidate -> Votes -> Votes
-}
