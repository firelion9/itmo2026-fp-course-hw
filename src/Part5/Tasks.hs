module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc (a:as) = myFoldl f (f acc a) as
myFoldl f acc []     = acc

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z (a:as) = f a $ myFoldr f z as
myFoldr f z []     = z 

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\a acc -> f a : acc) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr (\a acc -> f a ++ acc) []

myConcat :: [[a]] -> [a]
myConcat = myFoldr (++) []

myReverse :: [a] -> [a]
myReverse = myFoldl (flip (:)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\a acc -> if p a then a:acc else acc) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr (\a (as, bs) -> if p a then (a:as, bs) else (as, a:bs)) ([], [])

