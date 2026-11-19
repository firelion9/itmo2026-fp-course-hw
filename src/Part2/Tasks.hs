module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
infixl 6 |+|
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 6 |-|
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expr = case expr of
   IntConstant _              -> expr
   Variable v                 -> if v == varName then replacement else expr
   BinaryTerm op a b          -> BinaryTerm op (apply a) (apply b)
      where apply = replaceVar varName replacement

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expr = case expr of
   IntConstant _              -> expr
   Variable _                 -> expr
   BinaryTerm op a b          -> let 
      a' = evaluate a
      b' = evaluate b
      op' = case op of
         Plus  -> (+) 
         Minus -> (-) 
         Times -> (*)
      in case (a', b') of
         (IntConstant x, IntConstant y) -> IntConstant $ op' x y
         otherwise                      -> BinaryTerm op a' b'
