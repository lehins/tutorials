#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-7.7 runghc

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List as List
import Data.Monoid
import Text.Printf

-- | Social Security Number. Commonly used as a unique identification number of a
-- person.
data SSN = SSN
  { ssnPrefix :: Int
  , ssnInfix :: Int
  , ssnSuffix :: Int
  } deriving (Eq, Ord)

instance Show SSN where
  show (SSN p i s) = printf "%03d-%02d-%04d" p i s

data Gender = Male | Female deriving (Eq, Show)

data Person = Person
  { firstName :: String
  , lastName :: String
  , gender :: Gender
  } deriving (Eq)

instance Show Person where
  show (Person fName lName g) = fName ++ ' ':lName ++ " (" ++ show g ++ ")"

mkSSN :: Int -> Int -> Int -> SSN
mkSSN p i s
  | p <= 0 || p == 666 || p >= 900 = error $ "Invalid SSN prefix: " ++ show p
  | i <= 0 || i > 99 = error $ "Invalid SSN infix: " ++ show i
  | s <= 0 || s > 9999 = error $ "Invalid SSN suffix: " ++ show s
  | otherwise = SSN p i s


type Employees = Map.Map SSN Person

employees :: Employees
employees =
  Map.fromList
    [ (mkSSN 525 21 5423, Person "John" "Doe" Male)
    , (mkSSN 521 01 8756, Person "Mary" "Jones" Female)
    , (mkSSN 585 11 1234, Person "William" "Smith" Male)
    , (mkSSN 525 15 5673, Person "Maria" "Gonzalez" Female)
    , (mkSSN 524 34 1234, Person "Bob" "Jones" Male)
    , (mkSSN 522 43 9862, Person "John" "Doe" Male)
    , (mkSSN 527 75 1035, Person "Julia" "Bloom" Female)
    ]

lookupEmployee :: SSN -> Employees -> Maybe Person
lookupEmployee = Map.lookup



showMap :: (Show k, Show v) => Map.Map k v -> String
showMap = List.intercalate "\n" . map show . Map.toList



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

printEmployees :: Employees -> IO ()
printEmployees = putStrLn . showEmployees

showEmployeesBad :: Employees -> String
showEmployeesBad es
  | Map.null es = ""
  | otherwise = tail $ Map.foldrWithKey appender "" es
  where
    appender key person list = '\n' : (curry showEmployee) key person ++ list




-- Map.insert
-- | Adding an employee
-- λ> Map.insert (mkSSN 987 78 1323) (Person "Joanna" "Bloom") employees


-- | λ> printEmployees $ Map.map keepFirstInitial employees
keepFirstInitial :: Person -> Person
keepFirstInitial p@(Person (x:_) _ _) = p { firstName = x:"." }

-- Map.map
conciseEmployees :: Employees
conciseEmployees = Map.map keepFirstInitial employees


-- Map.insertWith
-- putStrLn $ showEmployees $ Map.insertWith keepFirstInitial (mkSSN 987 78 1323) (Person "Joanna" "Bloom") $ Map.map firstInitial employees


-- | Got married, changed her last name, let's make sure first name hasn't
-- changed, but add her as a new employee if she doesn't exist in db yet.
-- λ> printEmployees $ changedLastName (mkSSN 987 78 1323) (Person "Joanna" "Carter") employees
-- λ> printEmployees $ changedLastName (mkSSN 527 75 1035) (Person "Julia" "Carter") employees
changedLastName :: SSN -> Person -> Employees -> Employees
changedLastName = Map.insertWith checkFirstName
  where
    checkFirstName p1 p2
      | firstName p1 /= firstName p2 = error "Fisrt name has changed."
      | otherwise = p1 -- attention: the first argument is the new element.


-- | Exercise
showEmployeesM :: Employees -> String
showEmployeesM es
  | Map.null es = mempty
  | otherwise = showE firstSSN firstName <> Map.foldMapWithKey prepender rest
  where
    showE = curry showEmployee
    ((firstSSN, firstName), rest) = Map.deleteFindMin es
    prepender key person = '\n' : showE key person

-- | Exercise
showEmployeesReversed :: Employees -> String
showEmployeesReversed es
  | Map.null es = ""
  | otherwise = showE firstSSN firstName <> Map.foldlWithKey prepender "" rest
  where
    showE = curry showEmployee
    ((firstSSN, firstName), rest) = Map.deleteFindMax es
    prepender acc key person = '\n' : showE key person ++ acc


withinPrefixRangeNaive :: Int -> Int -> Employees -> Employees
withinPrefixRangeNaive prefixLow prefixHigh = Map.filterWithKey ssnInRange where
  ssnInRange (SSN prefix _ _) _ = prefix >= prefixLow && prefix <= prefixHigh


withinPrefixRange :: Int -> Int -> Employees -> Employees
withinPrefixRange prefixLow prefixHigh =
  fst . Map.split (SSN (prefixHigh + 1) 0 0) . snd . Map.split (SSN prefixLow 0 0)

employeesFromColorado :: Employees -> Employees
employeesFromColorado = withinPrefixRange 521 524



{-| 

### Union
-}

employeesFromNewMexico :: Employees -> Employees
employeesFromNewMexico es =
  withinPrefixRange 525 525 es `Map.union` withinPrefixRange 585 585 es


{-|

λ> printEmployees $ employeesFromNewMexico employees
525-15-5673: Maria Gonzalez (Female)
525-21-5423: John Doe (Male)
585-11-1234: William Smith (Male)


For compactness only states that are considered the South West are listed.
-}

data State =
  Arizona | California | Colorado | NewMexico | Nevada | Oklahoma | Texas | Utah -- ...
  deriving (Show, Eq, Ord, Enum)

statePrefixRangeMap :: Map.Map State [(Int, Int)]
statePrefixRangeMap =
  Map.fromAscList
    [ (Arizona, [(526, 527)])
    , (California, [(545, 573)])
    , (Colorado, [(521, 524)])
    , (NewMexico, [(525, 525), (585, 585)])
    , (Nevada, [(530, 530), (680, 680)])
    , (Oklahoma, [(440, 448)])
    , (Texas, [(449, 467)])
    , (Utah, [(528, 529)])
    ]
-- ...


-- | Map a state to a set of prefixes that correspond to it.
statePrefixMap :: Map.Map State (Set.Set Int)
statePrefixMap =
  Map.fromSet
    (Set.fromList . concatMap (uncurry enumFromTo) . (statePrefixRangeMap Map.!))
    allStates
  -- alternative:    
  -- Map.fromSet
  -- (Set.unions .
  --  map (Set.fromList . uncurry enumFromTo) . (statePrefixRangeMap Map.!))
  -- allStates



-- | Inverse Map of what we have above: from prefix to State
prefixStateMap :: Map.Map Int State
prefixStateMap = Map.foldlWithKey addPrefixes Map.empty statePrefixMap where
  addPrefixes spm state = Map.union spm . Map.fromSet (const state)


statePersonsMap :: Employees -> Map.Map State [Person]
statePersonsMap = Map.foldlWithKey updateState Map.empty
  where updateState ppsm ssn p =
          case Map.lookup (ssnPrefix ssn) prefixStateMap of
            Nothing    -> ppsm
            Just state -> Map.alter (consPerson p) state ppsm
        consPerson p Nothing = Just [p]
        consPerson p (Just ps) = Just (p : ps)


stateSocialsMap :: Employees -> Map.Map State (Set.Set SSN)
stateSocialsMap = Set.foldl updateState Map.empty . Map.keysSet
  where updateState ppsm ssn =
          case Map.lookup (ssnPrefix ssn) prefixStateMap of
            Nothing    -> ppsm
            Just state -> Map.alter (addSSN ssn) state ppsm
        addSSN ssn Nothing = Just $ Set.singleton ssn
        addSSN ssn (Just ssnSet) = Just $ Set.insert ssn ssnSet


employeesFrom :: State -> Employees -> Employees
employeesFrom state es = Map.unions $ map fromRange (statePrefixRangeMap Map.! state)
  where fromRange (low, high) = withinPrefixRange low high es


allStates :: Set.Set State
allStates = Set.fromAscList [toEnum 0 ..]

allStateEmployeesMap :: Employees -> Map.Map State Employees
allStateEmployeesMap es = Map.fromSet (`employeesFrom` es) allStates


statePersonsMap' :: Employees -> Map.Map State [Person]
statePersonsMap' = Map.map Map.elems . Map.filter (not . Map.null) . allStateEmployeesMap

stateSocialsMap' :: Employees -> Map.Map State (Set.Set SSN)
stateSocialsMap' = Map.mapMaybe nonEmptyElems . allStateEmployeesMap
  where nonEmptyElems sem | Map.null sem = Nothing
                          | otherwise = Just $ Map.keysSet sem


{-
Lets' check if we have at least one state that we are missing an employee from:

λ> Set.isProperSubsetOf (Map.keysSet $ allStatePersonMap employees) allStates
False
λ> Set.isProperSubsetOf (Map.keysSet $ statePersonMap employees) allStates
True
λ> Map.isProperSubmapOfBy (const . const True) (allStatePersonMap employees) statePrefixMap
False
λ> Map.isProperSubmapOfBy (const . const True) (statePersonMap employees) statePrefixMap
True

-}







{- |

### Difference


All employees that are not from New Mexico.

λ> printEmployees (employees Map.\\ employeesFrom NewMexico employees)
521-01-8756: Mary Jones (Female)
522-43-9862: John Doe (Male)
524-34-1234: Bob Jones (Male)
527-75-1035: Julia Bloom (Female)

-}


