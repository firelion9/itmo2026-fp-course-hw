module Part6.Tests where

import qualified Data.Map

import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Part6.Tasks

unit_eye = do
    eye 1 @?= one
    eye 1 @?= [[one]]
    eye 1 @?= SparseMatrix 1 1 (Data.Map.fromList [((0, 0), one)])
    eye 2 @?= [[one, 0], [0, one]]
    eye 2 @?= SparseMatrix 2 2 (Data.Map.fromList [((0, 0), one), ((1, 1), one)])

    where one :: Int; one = 1

unit_zero = do
    zero 1 1 @?= zz
    zero 2 1 @?= [[zz, zz]]
    zero 2 2 @?= [[zz, zz], [zz, zz]]
    zero 5 5 @?= SparseMatrix 5 5 (Data.Map.fromList ([]::[((Int, Int), Int)]))
    where zz :: Int; zz = 0


unit_multiply = do
    multiplyMatrix [[3 :: Int]] [[4]] @?= [[12]]
    multiplyMatrix [[1 :: Int,2],[3,4]] [[5,6],[7,8]] @?= [[19,22],[43,50]]
    multiplyMatrix [[1 :: Int,2],[3,4]] [[0,0],[0,0]] @?= [[0,0],[0,0]]
    multiplyMatrix [[1 :: Int,2],[3,4]] [[1,0],[0,1]] @?= [[1,2],[3,4]]
    multiplyMatrix [[1 :: Int,0],[0,1]] [[1,2],[3,4]] @?= [[1,2],[3,4]]
    multiplyMatrix [[1 :: Int,2,3],[4,5,6]] [[7,8],[9,10],[11,12]]
            @?= [[58,64],[139,154]]
    multiplyMatrix [[1 :: Int,2],[3,4],[5,6]] [[7,8,9],[10,11,12]]
            @?= [[27,30,33],[61,68,75],[95,106,117]]
    multiplyMatrix [[1 :: Int,2,3]] [[4],[5],[6]] @?= [[32]]
    multiplyMatrix [[1 :: Int],[2],[3]] [[4,5,6]] @?= [[4,5,6],[8,10,12],[12,15,18]]
    multiplyMatrix [[1 :: Int,0,2],[-1,3,1],[2,1,1]] [[3,1,1],[-1,2,1],[2,1,2]]
            @?= [[7,3,5],[-4,6,4],[7,5,5]]
    multiplyMatrix [[-1 :: Int,2],[3,-4]] [[5,-6],[-7,8]] @?= [[-19,22],[43,-50]]
    multiplyMatrix [[0 :: Int,1],[0,0]] [[0,0],[1,0]] @?= [[1,0],[0,0]]
    multiplyMatrix [[0 :: Int,0],[1,0]] [[0,1],[0,0]] @?= [[0,0],[0,1]]

unit_determinant = do
  determinant [[5 :: Int]] @?= 5
  determinant [[-7 :: Int]] @?= -7
  determinant [[1 :: Int,2],[3,4]] @?= -2
  determinant [[3 :: Int,8],[4,6]] @?= -14
  determinant [[-2 :: Int,1],[1,-2]] @?= 3
  determinant [[0 :: Int,1],[1,0]] @?= -1
  determinant [[1 :: Int,2],[2,4]] @?= 0
  determinant [[2 :: Int,0],[0,3]] @?= 6
  determinant [[1 :: Int,0,0],[0,1,0],[0,0,1]] @?= 1
  determinant [[1 :: Int,2,3],[0,4,5],[0,0,6]] @?= 24
  determinant [[1 :: Int,2,3],[0,0,0],[4,5,6]] @?= 0
  determinant [[2 :: Int,-3,1],[2,0,-1],[1,4,5]] @?= 49
  determinant [[6 :: Int,1,1],[4,-2,5],[2,8,7]] @?= -306
  determinant [[1 :: Int,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]] @?= 1
  determinant [[1 :: Int,0,0,0],[0,2,0,0],[0,0,3,0],[0,0,0,4]] @?= 24
