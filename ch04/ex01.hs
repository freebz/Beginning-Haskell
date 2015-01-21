--
-- Chapter 4 Using Containers and Type Classes
--

--
-- Using Packages
--

--
-- Managing Packages with Cabal and EclipseFP
--

--
-- Containers: Maps, Sets, Trees, Graphs
--

--
-- Maps
--

--import qualified Data.Map as M

--
-- Tree
--

import Data.Tree

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees) = 
         let subtreesTraversed = concat $ map (preOrder f) subtrees
         in f v : subtreesTraversed

pictureTree :: Tree Int
pictureTree = Node 1 [ Node 2 [ Node 3 []
                              , Node 4 []
                              , Node 5 [] ]
                     , Node 6 [] ]


