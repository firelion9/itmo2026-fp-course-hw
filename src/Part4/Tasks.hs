module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist as = go as REmpty where
    go [] acc = acc
    go (a:as) acc = go as (acc :< a)

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    showsPrec _ REmpty = (++"[]")
    showsPrec _ as = ("["++) . (go as ("]"++))  where
        go (as:<a) acc = go' as ((showHead a) . acc)
        go' as acc = foldr (\a acc -> (showTail a) . acc) acc as
        delim = (","++)
        showA = showsPrec 0
        showHead = showA
        showTail a = (showA a) . delim

instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty   = True
    (==) _ REmpty        = False
    (==) REmpty _        = False
    (==) (as:<a) (bs:<b) = a == b && as == bs

instance Semigroup (ReverseList a) where
    (<>) as (bs:<b) = (as <> bs) :< b
    (<>) as REmpty  = as

instance Monoid (ReverseList a) where
    mempty = REmpty

instance Foldable ReverseList where
    foldr f acc (as:<a) = foldr f (f a acc) as
    foldr _ acc REmpty  = acc

instance Functor ReverseList where
    fmap f (as:<a) = (fmap f as) :< (f a)
    fmap f REmpty  = REmpty

instance Applicative ReverseList where
    pure = (REmpty:<)
    (<*>) fs as = foldr (\f acc -> (fmap f as) <> acc) REmpty fs

instance Monad ReverseList where
    (>>=) as f = foldr (\a acc -> (f a) <> acc) REmpty as
