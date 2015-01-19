{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}


-- More on Modules
--

--import Data.List
--import Data.List (permutations, subsequences)
--import Data.List hiding (head, tail)
--import qualified Data.List (permutations, subsequences)
--import qualified Data.List as L
import qualified Data.List as L (permutations, subsequences)
import Data.List

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter = filter (\l -> head l == letter) . 
                                  L.permutations



-- Smart Constructors and Views

data Range = Range Integer Integer deriving Show

range :: Integer -> Integer -> Range
range a b = if a <= b then Range a b
            else error "a must be <= b"


data RangeObs = R Integer Integer deriving Show

r :: Range -> RangeObs
r (Range a b) = R a b


prettyRange :: Range -> String
prettyRange rng = case rng of
                       (r -> R a b) -> "[" ++ show a ++ "," ++ show b ++ "]"



--
-- Diving into Lists
--

-- Folds


data InfNumber a = MinusInfinity
                 | Number a
                 | PlusInfinity
                 deriving Show

infMax MinusInfinity x    = x
infMax x MinusInfinity    = x
infMax PlusInfinity _     = PlusInfinity
infMax _ PlusInfinity     = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)



--
-- Lists and Predicates
--

bothFilters :: (a -> Bool) -> [a] -> ([a],[a])
bothFilters p list = (filter p list, filter (not . p) list)



data Client = GovOrg        String
            | Company       String Integer String String
            | Individual    Person Bool
            deriving Show

data Person = Person String String Gender
              deriving Show

data Gender = Male | Female | Unknown
              deriving Show

{-
skipUntilGov :: [Client a] -> [Client a]
skipUntilGov = dropWhile (\case { GovOrg {} -> False;
                                  _         -> True })
isIndividual :: Client a -> Bool
isIndividual (Individual {}) = True
isIndividual _               = False
-}

--elem' :: a -> [a] -> Bool
elem' :: (Eq a) => a -> [a] -> Bool
elem' x list = case Data.List.find (==x) list of
                    Nothing -> False
                    _       -> True

{-

compareClient :: Client a -> Client a -> Ordering
compareClient (Individual{person = p1}) (Individual{person = p2})
              = compare (firstName p1) (firstName p2)
compareClient (Individual {} ) _ = GT
compareClient _ (Individual {} ) = LT
compareClient c1 c2              = compare (clientName c1) (clientName c2)

-}



--
-- List Containing Tuples
--

enum :: Int -> Int -> [Int]
enum a b | a > b = []
enum a b         = a : enum (a+1) b

withPositions :: [a] -> [(Int,a)]
--withPositions list = zip (enum 1 $ length list) list
--withPositions list = zip [1 .. length list] list
withPositions list = zip [1 .. ] list



--
-- List Comprehensions
--

doubleOdds :: [Integer] -> [Integer]
--doubleOdds list = map (*2) $ filter odd list
doubleOdds list = [2 * x | x <- list, odd x]




--
-- Haskell Origami
--


minSort :: [Integer] -> [Integer]
minSort = unfoldr (\case [] -> Nothing
                         xs -> Just (m, delete m xs) where m = minimum xs)


flodr2 :: (Maybe (a,b) -> b) -> [a] -> b
foldr2 f []      = f Nothing
flodr2 f (x:xs)  = f $ Just (x, foldr2 f xs)

mapAsFold2 :: (a -> b) -> [a] -> [b]
mapAsFold2 f = foldr2 (\case Nothing            -> []
                             Just (x, xs)       -> f x : xs)
