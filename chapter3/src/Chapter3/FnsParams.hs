{-# LANGUAGE LambdaCase #-}

module Chapter3.FnsParams where

import Chapter3.ParamPoly (Client(..), duplicate)

filterOnes :: [Integer] -> [Integer]
filterOnes = filter (\x -> x == 1)

filterANumber :: Eq a => [a]  -> a -> [a]
filterANumber lst number =  filter (\x -> x == number) lst

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot _ [] = []
filterNot f (x:xs) = if f x
                             then filterNot f xs
                             else x : (filterNot f xs)

isGovOrg :: Client i -> Bool
isGovOrg (GovOrg {}) = True
isGovOrg _ = False

filterGovOrgs :: [Client i] -> [Client i]
filterGovOrgs = filter isGovOrg

(***) :: (a -> b) -> (c -> d) -> ((a,c) -> (b,d))
f *** g = \(x,y) -> (f x, g y)

formula1 :: Integer -> Integer
formula1 = uncurry (+) .  ((* 3) *** ((* 7) . (+ 2))) . duplicate
