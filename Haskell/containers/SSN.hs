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


employeesFromNewMexico :: Employees -> Employees
employeesFromNewMexico es =
  withinPrefixRange 525 525 es `Map.union` withinPrefixRange 585 585 es


data State
  = Alabama
  | Alaska
  | Arizona
  | Arkansas
  | California
  | Colorado
  | Connecticut
  | Delaware
  | Florida
  | Georgia
  | Hawaii
  | Idaho
  | Illinois
  | Indiana
  | Iowa
  | Kansas
  | Kentucky
  | Louisiana
  | Maine
  | Maryland
  | Massachusetts
  | Michigan
  | Minnesota
  | Mississippi
  | Missouri
  | Montana
  | Nebraska
  | Nevada
  | NewHampshire
  | NewJersey
  | NewMexico
  | NewYork
  | NorthCarolina
  | NorthDakota
  | Ohio
  | Oklahoma
  | Oregon
  | Pennsylvania
  | RhodeIsland
  | SouthCarolina
  | SouthDakota
  | Tennessee
  | Texas
  | Utah
  | Vermont
  | Virginia
  | Washington
  | WestVirginia
  | Wisconsin
  | Wyoming
  deriving (Show, Eq, Ord, Enum)

statePrefixRangeMap :: Map.Map State [(Int, Int)]
statePrefixRangeMap =
  Map.fromList
    [ (NewHampshire, [(1, 3)])
    , (Maine, [(4, 7)])
    , (Vermont, [(8, 9)])
    , (Massachusetts, [(10, 34)])
    , (RhodeIsland, [(35, 39)])
    , (Connecticut, [(40, 49)])
    , (NewYork, [(50, 134)])
    , (NewJersey, [(135, 158)])
    , (Pennsylvania, [(159, 211)])
    , (Maryland, [(212, 220)])
    , (Delaware, [(221, 222)])
    , (Virginia, [(223, 231)])
    , (NorthCarolina, [(232, 232)])
    , (WestVirginia, [(232, 236)])
    , (SouthCarolina, [(247, 251)])
    , (Georgia, [(252, 260)])
    , (Florida, [(261, 267)])
    , (Ohio, [(268, 302)])
    , (Indiana, [(303, 317)])
    , (Illinois, [(318, 361)])
    , (Michigan, [(362, 386)])
    , (Wisconsin, [(387, 399)])
    , (Kentucky, [(400, 407)])
    , (Tennessee, [(408, 415)])
    , (Alabama, [(416, 424)])
    , (Mississippi, [(425, 428)])
    , (Arkansas, [(429, 432)])
    , (Louisiana, [(433, 439)])
    , (Oklahoma, [(440, 448)])
    , (Texas, [(449, 467)])
    , (Minnesota, [(468, 477)])
    , (Iowa, [(478, 485)])
    , (Missouri, [(486, 500)])
    , (NorthDakota, [(501, 502)])
    , (SouthDakota, [(503, 504)])
    , (Nebraska, [(505, 508)])
    , (Kansas, [(509, 515)])
    , (Montana, [(516, 517)])
    , (Idaho, [(518, 519)])
    , (Wyoming, [(520, 520)])
    , (Colorado, [(521, 524)])
    , (NewMexico, [(525, 525), (585, 585)])
    , (Arizona, [(526, 527)])
    , (Utah, [(528, 529)])
    , (Nevada, [(530, 530), (680, 680)])
    , (Washington, [(531, 539)])
    , (Oregon, [(540, 544)])
    , (California, [(545, 573)])
    , (Alaska, [(574, 574)])
    , (Hawaii, [(575, 576)])
    ]


-- | Map a state to a set of prefixes that correspond to it.
statePrefixMap :: Map.Map State (Set.Set Int)
statePrefixMap =
  Map.fromSet
    (Set.fromList . concatMap (uncurry enumFromTo) . (statePrefixRangeMap Map.!))
    allStates


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


symmetricDifference :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
symmetricDifference a b = Set.union a b Set.\\ Set.intersection a b

