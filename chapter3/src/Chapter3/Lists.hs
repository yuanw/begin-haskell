{-# LANGUAGE LambdaCase #-}

module Chapter3.Lists where

import Data.List (find)
import Data.Ord

import Chapter3.ParamPoly

data InfNumber a = MinusInfinity
                 | Number a
                 | PlusInfinity
                 deriving Show

infMax MinusInfinity x = x
infMax x MinusInfinity = x
infMax PlusInfinity _ = PlusInfinity
infMax _ PlusInfinity = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)

product' :: [Integer] -> Integer
product' = foldl (*) 1

all' :: [Bool] -> Bool
all' = foldl (&&) True

minimumBy' :: (Integer -> Integer) -> [Integer] -> Integer
minimumBy' func numbers = foldl min 0 (map func numbers)

bothFilters :: (a -> Bool) -> [a] -> ([a], [a])
bothFilters p list = (filter p list, filter (not . p) list)

elem' :: Eq a => a -> [a] -> Bool
elem' element lst = case find (\x -> x == element) lst of
    Nothing -> False
    Just _ -> True

skipUntilGov :: [Client a] -> [Client a]
skipUntilGov = dropWhile (\case { GovOrg {} -> False ; _ -> True })

