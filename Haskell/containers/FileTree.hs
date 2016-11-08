#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-7.7 runghc --package containers

import qualified Data.Map as Map


data FileTree a = File a
                | Directory (Map.Map String (FileTree a))
