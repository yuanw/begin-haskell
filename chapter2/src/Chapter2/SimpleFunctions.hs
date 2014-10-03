{-# LANGUAGE ViewPatterns #-}
module Chapter2.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

--(+++) :: [a] -> [a] -> [a]
--lst1 +++ lst2 = if null lst1
--                        then lst2
--                        else (head lst1): (tail lst1 +++ lst2)

(+++) :: [a] -> [a] -> [a]
list1 +++ list2 = case list1 of
                    [] -> list2
                    x:xs -> x : (xs +++ list2)

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x: r@(y:_)) = x < y && sorted(r)

reverse2 :: [a] -> [a]
reverse2 lst = if null lst
                     then lst
                     else reverse2(tail lst) +++ [head lst]

maxmin :: Ord a => [a] -> (a, a)
maxmin [x] = (x,x)
maxmin (x:xs) = (if x > xs_max then x else xs_max,
                if x < xs_min then x else xs_min
                ) where (xs_max, xs_min) = maxmin xs

ackermann :: Integer -> Integer -> Maybe Integer
ackermann m n | m == 0 = Just (n + 1)
ackermann m n | m > 0 && n == 0 = ackermann (m-1) 1
ackermann m n | m > 0 && n > 0 = let Just f1 = ackermann m (n-1)
                                  in ackermann (m-1) f1
ackermann _ _ | otherwise = Nothing

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' [(x, y)] = ([x], [y])
unzip' ((x, y) : zs) = (x:resultFirst, y:resultSecond)
                      where result = unzip zs
                            resultFirst = fst result
                            resultSecond = snd result



data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
    deriving Show

data Gender = Male | Female | Unknown
   deriving Show

data ClientR = GovOrgR {clientRName :: String}
             | CompanyR {clientRName :: String
                        , companyId :: Integer
                        , person:: PersonR
                        , duty:: String}
             | IndividualR { person :: PersonR}
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       } deriving Show


clientName :: Client -> String
clientName client = case client of
          GovOrg name          -> name
          Company name _ _ _  -> name
          Individual p _ ->
             case p of Person fName lName _ -> fName ++ " " ++ lName

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False

data TimeMachine = TimeMachine Manufacturer Integer String TimeTravel Float
data Manufacturer = Manufacturer String
data TimeTravel = TimeTravel Bool Bool

countGender :: [Client] -> (Integer, Integer)
countGender clients = if null clients
                      then (0, 0)
                      else
                            case client of
                                 Individual p _ ->
                                    case p of Person _  _ Male -> (male + 1, female)
                                              Person _ _ Female -> (male, female + 1)
                                 _ -> result
                            where tailClients = (tail clients)
                                  client = head clients
                                  result = countGender tailClients
                                  male = fst result
                                  female = snd result


--decreasePrice :: [TimeMachine] -> percentage -> [TimeMachine]
--decreasePrice lst = if null lst
--                     then lst
--                     else decreasePrice(tail lst) +++ [head lst]
--                        case machine
--                     where machine = head lst


