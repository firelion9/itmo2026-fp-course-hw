{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import qualified Data.Map as Map
import qualified Data.List (transpose)
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map.Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    width  :: mx -> Int
    height :: mx -> Int
    zero   :: Int -> Int -> mx
    initMx   :: Int -> Int -> (Int -> Int -> Int) -> mx
    transpose :: mx -> mx
    toRows    :: mx -> [[Int]]
    fromRows :: [[Int]] -> mx

    zero w h = initMx w h (\_ _ -> 0)
    initMx w h f = fromRows [[f y x | x <- [0..w-1]] | y <- [0..h-1]]

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    width _  = 1
    height _ = 1

    transpose = id
    toRows m  = [[m]]
    fromRows  = head . head

instance Matrix [[Int]] where
    width  = length . head
    height = length

    transpose = Data.List.transpose
    toRows    = id
    fromRows  = id

instance Matrix (SparseMatrix Int) where
    width  = sparseMatrixWidth
    height = sparseMatrixHeight

    transpose (SparseMatrix w h m) = SparseMatrix h w $ Map.mapKeys swap $ m
    toRows    (SparseMatrix w h m) = [[fromMaybe 0 $ Map.lookup ((y, x) :: (Int, Int)) m | x <- [0..w-1]] | y <- [0..h-1]]
    fromRows rs = SparseMatrix (length $ head rs) (length rs) $ Map.fromList $ [(k, v) | as <- indexed2d rs, (k, v) <- as, v /= 0] where
        indexed2d = map (\(x, as) -> map (\(y, a) -> ((x, y), a)) $ indexed as) . indexed
        indexed = indexed' 0
        indexed' _ []     = []
        indexed' n (a:as) = (n, a) : indexed' (n+1) as

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = initMx w w (\y x -> if x == y then 1 else 0)
-- Матрица, заполненная нулями
-- zero :: Matrix m => Int -> Int -> m
-- zero w h = notImplementedYet
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix a b = fromRows crs where
    bt = transpose b
    bcs = toRows bt
    ars = toRows a
    crs = [[sum $ map (uncurry (*)) $ zip ar bc | bc <- bcs] | ar <- ars]
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant m | width m /= height m = error "cannot compute determinant of non-square matrix"
              | height m == 0       = 0
              | otherwise           = determinant' $ toRows m
                where
                 determinant' rs = if height rs == 1 then head $ head $ rs else determinant' nextRows `div` mul where
                    fc  = head $ Data.List.transpose rs
                    elimSub :: [Int] -> [Int] -> [Int]
                    elimSub (b:base) (r:row) = map (\(b', r') -> r' * mr - b' * mb) $ zip base row where
                        mb = r
                        mr = b

                    nextRows = map (elimSub $ head rs) (tail rs)
                    mul  = (head $ head rs) ^ (width rs - 2)
