{-# LANGUAGE ViewPatterns #-}
{- # LANGUAGE Extension #-}

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

lst1 +++ lst2 = if null lst1 {- check emptyness -}
              then lst2      -- base case
              else (head lst1) : (tail lst1 +++ lst2)

reverse2 list = if null list
                then []
                else reverse2 (tail list) +++ [head list]

maxmin list = if null (tail list)
              then (head list, head list)
              else ( if (head list) > fst (maxmin (tail list))
                     then head list
                     else fst (maxmin (tail list))
                   , if (head list) < snd (maxmin (tail list))
                     then head list
                     else snd (maxmin (tail list))
                   )

maxmin2 list = let h = head list
              in if null (tail list)
                 then (h, h)
                 else ( if h > t_max then h else t_max
                      , if h < t_min then h else t_min )
                      where t = maxmin2 (tail list)
                            t_max = fst t
                            t_min = snd t

data Client = GovOrg        String
            | Company       String Integer String String
            | Individual    Person Bool
            deriving Show

data Person = Person String String Gender
              deriving Show

data Gender = Male | Female | Unknown
              deriving Show

{-
clientName :: Client -> String
clientName client = case client of
                         GovOrg name                 -> name
                         Company name id person resp -> name
                         Individual person ads       ->
                             case person of Person fName lName gender -> fName ++ " " ++ lName
-}

clientName :: Client -> String
clientName client = case client of  -- other patterns
                     Individual (Person fName lName _) _ -> fName ++ " " ++ lName

{-
companyName :: Client -> String
companyName client = case client of
                     Company name _ _ _ -> name
-}

companyName :: Client -> Maybe String
companyName client = case client of
                     Company name _ _ _ -> Just name
                     _                  -> Nothing



-- Guards

{-
ifibonacci :: Integer -> Maybe Integer
ifibonacci n = if n < 0
               then Nothing
               else case n of
                         0 -> Just 0
                         1 -> Just 1
                         n -> let Just f1 = ifibonacci (n-1)
                                  Just f2 = ifibonacci (n-2)
                                  in Just (f1 + f2)

binom _ 0 = 1
binom x x = 1
binom n k = (binom (n-1) (k-1)) + (binom (n-1) k)
-}

ifibonacci n | n < 0    = Nothing
ifibonacci 0            = Just 0
ifibonacci 1            = Just 1
ifibonacci n | otherwise = let (Just f1, Just f2) = (ifibonacci (n-1), ifibonacci(n-2))
                           in Just (f1 + f2)

binom _ 0          = 1
binom x y | x == y = 1
binom n k          = (binom (n-1) (k-1)) + (binom (n-1) k)


multipleOf :: Integer -> Integer -> Bool
multipleOf x y = (mod x y) == 0

specialMultiples :: Integer -> String
{-
specialMultiples n | multipleOf n 2 = show n ++ " is multiple of 2"
specialMultiples n | multipleOf n 3 = show n ++ " is multiple of 3"
specialMultiples n | multipleOf n 5 = show n ++ " is multiple of 5"
specialMultiples n | otherwise      = show n ++ " is a beautiful number"
-}
specialMultiples n
  | multipleOf n 2 = show n ++ " is multiple of 2"
  | multipleOf n 3 = show n ++ " is multiple of 3"
  | multipleOf n 5 = show n ++ " is multiple of 5"
  | otherwise      = show n ++ " is beautiful number"


-- View Patterns

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director" ) = True
specialClient _                               = False



-- Records
-- Creation and Use

data ClientR = GovOrgR { clientRName :: String }
             | CompanyR { clientRName :: String
                        , companyId :: Integer
                        , person :: PersonR
                        , duty :: String }
             | IndividualR { person :: PersonR }
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       } deriving Show


greet :: ClientR -> String
greet IndividualR { person = PersonR { firstName = fn } } = "Hi, " ++ fn
greet CompanyR    { clientRName = c }                     = "Hello, " ++ c
greet GovOrgR     { }                                     = "Welcome"

{-
greet IndividualR { person = PersonR { firstName } } = "Hi, " ++ firstName
greet CompanyR    { clientRName }                    = "Hello, " ++ clientRName
greet GovOrgR     { }                                = "Welcome"
-}


nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) =
               let newName = (toUpper initial):rest
               in p { firstName = newName }
nameInCapitals p@(PersonR { firstName = "" }) = p




-- The "Default Values" Idiom

data ConnType = TCP | UDP
data UseProxy = NoProxy | Proxy String
data TimeOut  = NoTimeOut | TimeOut Integer

data Connection = ... -- Definition omitted
connect :: String -> ConnType -> Integer -> UseProxy -> Bool -> Bool -> TimeOut -> Connection

connectUrl :: String -> Connection
connectUrl u = connect u TCP 0 NoProxy False False NoTimeOut

data ConnOptions = ConnOptions { connType    :: ConnType
                               , connSpeed   :: Integer
                               , connProxy   :: UseProxy
                               , connCaching :: Bool
                               , connKeepAlive  :: Bool
                               , connTimeOut    :: TimeOut
                               }

connect' :: String -> ConnOptions -> Connection
connect' url options = ...

connDefault :: ConnOptions
connDefault = ConnOptions TCP 0 NoProxy False False NoTimeOut
