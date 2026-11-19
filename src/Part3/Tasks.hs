module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = (f n) : finc f (n+1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq as = go as (replicate 10 0) where
    modifyAt (a:as) 0 f = (f a) : as 
    modifyAt (a:as) i f = a : (modifyAt as (i - 1) f)

    digits x | x < 0     = digits (-x)
             | x < 10    = [x]
             | otherwise = let (y, d) = divMod x 10 in d : digits y

    go :: [Int] -> [Int] -> Int
    go [] acc = let
        index :: [(Int, Int)]
        index = zip (finc id 0) acc
        folder :: (Int, Int) -> (Int, Int) -> (Int, Int)
        folder n@(i, cnt) p@(mi, mcnt) = if cnt > mcnt then n else p
        in fst $ foldr folder (-1, -1) index
    go (a:as) acc = go as (foldr (\x acc -> modifyAt acc x (+1)) acc (digits a))  

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = foldr add [] where
    add x [] = [x]
    add x (a:as) | a == x    = a:as
                 | otherwise = a:(add x as)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = go l [] where
    add [] x v = [(x, [v])]
    add ((k,vs):as) x v | k == x    = (k, v : vs) : as
                        | otherwise = (k,vs):(add as x v) 

    go [] res = res
    go (a:as) res = go as $ add res (f a) a 
