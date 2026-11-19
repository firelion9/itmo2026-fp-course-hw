module Part1.Tasks where

import Util(notImplementedYet)
import Data.List
import GHC.Float (int2Double)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = approx y 1.0 0.0 where
    y = x - 2 * pi * (int2Double $ floor (x / (2 * pi)))
    approx a n s = let
        a' = -a * y * y / (n + 1) / (n + 2)
      in if (abs a) < 0.00000001 then s else approx a' (n + 2) (s + a)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = approx 1.0 0.0 0.0 where
    y = x - 2 * pi * (int2Double $ floor (x / (2 * pi)))
    approx a n s = let
        a' = -a * y * y / (n + 1) / (n + 2)
      in if (abs a) < 0.00000001 then s else approx a' (n + 2) (s + a)


-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD 0 b = abs b
myGCD a b = myGCD (rem b a) a

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y = if d < 1 || m > 12 || m < 0 then False else check m where
    checkFeb = if y `rem` 400 == 0 || y `rem` 4 == 0 && y `rem` 100 /= 0 then d <= 29 else d <= 28
    checkShort = d <= 30
    checkLong = d <= 31
    check 1  = checkLong
    check 2  = checkFeb
    check 3  = checkLong
    check 4  = checkShort
    check 5  = checkLong
    check 6  = checkShort
    check 7  = checkLong
    check 8  = checkLong
    check 9  = checkShort
    check 10 = checkLong
    check 11 = checkShort
    check 12 = checkLong

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow a p | p < 0     = if (abs a) == 1 then go a (-p) 1 else 0
          | otherwise = go a p 1 where
            go a p acc | p == 0     = acc 
                       | even p     = go (a * a) (p `div` 2) acc 
                       | otherwise  = go (a * a) (p `div` 2) (acc * a)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime p = go p 2 where
            go p q | q * q > p  = True
                   | p `rem` q == 0 = False
                   | otherwise  = go p (q + 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea [] = 0
shapeArea [_] = 0
shapeArea [_, _] = 0
shapeArea as = abs s where
    pairs = zip as (drop 1 $ cycle as)
    trs = map (\((x1, y1), (x2, y2)) -> x1 * y2 - x2 * y1) pairs
    s = (sum trs) / 2 

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c | nonTr     = -1
                   | cs == 0.0 = 2
                   | cs < 0    = 1
                   | cs > 0    = 0 where
    [a', b', c'] = sort [a, b, c]
    nonTr = c' >= a' + b' || a' <= 0
    cs = c' * c' - (a' * a' + b' * b')
