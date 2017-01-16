#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-7.7 runghc

import qualified Data.Set as Set
{-
# Sets

-}

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
  deriving (Show, Eq, Enum)

foo = [
  ([(001,003)], NewHampshire)
  ,([(004,007)], Maine)
  ,([(008,009)], Vermont)
  ,([(010,034)], Massachusetts)
  ,([(035,039)], RhodeIsland)
  ,([(040,049)], Connecticut)
  ,([(050,134)], NewYork)
  ,([(135,158)], NewJersey)
  ,([(159,211)], Pennsylvania)
  ,([(212,220)], Maryland)
  ,([(221,222)], Delaware)
  ,([(223,231)], Virginia)
  ,([(232,232)], NorthCarolina)
  ,([(232,236)], WestVirginia)
  ,([(247,251)], SouthCarolina)
  ,([(252,260)], Georgia)
  ,([(261,267)], Florida)
  ,([(268,302)], Ohio)
  ,([(303,317)], Indiana)
  ,([(318,361)], Illinois)
  ,([(362,386)], Michigan)
  ,([(387,399)], Wisconsin)
  ,([(400,407)], Kentucky)
  ,([(408,415)], Tennessee)
  ,([(416,424)], Alabama)
  ,([(425,428)], Mississippi)
  ,([(429,432)], Arkansas)
  ,([(433,439)], Louisiana)
  ,([(440,448)], Oklahoma)
  ,([(449,467)], Texas)
  ,([(468,477)], Minnesota)
  ,([(478,485)], Iowa)
  ,([(486,500)], Missouri)
  ,([(501,502)], NorthDakota)
  ,([(503,504)], SouthDakota)
  ,([(505,508)], Nebraska)
  ,([(509,515)], Kansas)
  ,([(516,517)], Montana)
  ,([(518,519)], Idaho)
  ,([(520,520)], Wyoming)
  ,([(521,524)], Colorado)
  ,([(525,525),(585,585)], NewMexico)
  ,([(526,527)], Arizona)
  ,([(528,529)], Utah)
  ,([(530,530),(680,680)], Nevada)
  ,([(531,539)], Washington)
  ,([(540,544)], Oregon)
  ,([(545,573)], California)
  ,([(574,574)], Alaska)
  ,([(575,576)], Hawaii)
  ]


bar =
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

allStates :: Set.Set State
allStates = Set.fromAscList [toEnum 0 ..]


