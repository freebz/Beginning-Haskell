maybeString (Just _) = "Just"
maybeString Nothing  = "Nothing"

data Client i = GovOrg { clientId :: i, clientName :: String}
              | Company { clientId   :: i
                        , clientName :: String
                        , person     :: Person
                        , duty       :: String
                        }
              | Individual { clientId :: i, person :: Person }
              deriving Show

data Person = Person { firstName :: String, lastName :: String }
              deriving Show

data Triple a b c = Triple a b c

data SamePair a = SamePair a a


swapTriple (x, y, z) = (y, z, x)
duplicate x = (x, x)
nothing _ = Nothing

index [] = []
index [x] = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
               in (n+1, x):indexed


maybeA [] = 'a'



-- Higher-Order Functions

-- 3f(x + 2)
apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f (x + 2)




-- Anonymous Functions

equalTuples :: [(Integer,Integer)] -> [Bool]
equalTuples t = map (\(x,y) -> x == y) t

sayHello :: [String] -> [String]
sayHello names = map (\name -> case name of
                                    "Alejandro" -> "Hello, writer"
                                    _           -> "Welcome, " ++ name
                     ) names

{-
sayHello names = map (\case "Alejandro" -> "Hello, writer"
                            name        -> "Welcome, " ++ name
                     ) names
-}

multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN n = \x -> n*x



-- Partial Application of a Function
{-
double list = map (\x -> x * 2) list
double = \list -> map (\x -> x * 2) list
double = map (\x -> x * 2)
-}
double = map (*2)

{-
duplicateOdds list = map (*2) $ filter odd list
-}
duplicateOdds = map (*2) . filter odd

uncurry :: (a -> b -> c) -> (a ,b) -> c
uncurry f = \(x,y) -> f x y

curry :: ((a,b) -> c) -> a -> b -> c
curry f = \x y -> f (x,y)


(***) :: (a -> b) -> (c -> d) -> ((a,c) -> (b,d))
f *** g = \(x,y) -> (f x, g y)



-- 3x + 7(x+2)
formula1 :: Integer -> Integer
formula1 = uncurry (+) . ( ((*7) . (+2)) *** (*3) ) . duplicate

