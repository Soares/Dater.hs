module Data.List.Zipper where
import Control.Applicative hiding (empty)
import Control.Monad
import Data.Foldable hiding (toList, find)
import Data.List hiding (length, foldl, foldr, find, concatMap, insert)
import qualified Data.List as List
import Data.Maybe
import Data.Traversable
import Prelude hiding (length, foldl, foldr, concatMap)
import qualified Prelude

data Zipper a = Zip [a] [a]

backwards :: ([a] -> [b]) -> [a] -> [b]
backwards f xs = reverse $ f $ reverse xs

instance Show a => Show (Zipper a) where
    show (Zip xs ys) = show (reverse xs) ++ "^" ++ show ys
instance Functor Zipper where
    fmap f (Zip xs ys) = Zip (backwards (map f) xs) (map f ys)
instance Applicative Zipper where
    pure a = Zip [] [a]
    (Zip fs gs) <*> (Zip xs ys) = Zip
        (backwards (zipWith ($) fs) xs) (zipWith ($) gs ys)
instance Monad Zipper where
    return = pure
    (Zip la ra) >>= a2zb = Zip exl exr where
        exl = concatMap (reverse . toList . a2zb) la
        exr = concatMap (toList . a2zb) ra
instance Foldable Zipper where
    foldr f z (Zip xs ys) = foldl (flip f) (foldr f z ys) xs
instance Traversable Zipper where
    traverse f (Zip xs ys) = Zip <$>
        (reverse <$> traverse f (reverse xs)) <*> traverse f ys

empty :: Zipper a
empty = Zip [] []

fromList, fromListEnd :: [a] -> Zipper a
fromList = Zip []
fromListEnd as = Zip (reverse as) []

toList :: Zipper a -> [a]
toList (Zip xs ys) = reverse xs ++ ys

atEdge, atStart, atEnd, isEmpty :: Zipper a -> Bool
atEnd (Zip _ []) = True
atEnd _ = False
atStart (Zip [] _) = True
atStart _ = False
atEdge x = atStart x || atEnd x
isEmpty x = atStart x && atEnd x

toStart, toEnd :: Zipper a -> Zipper a
toStart = Zip [] . toList
toEnd (Zip xs ys) = Zip (reverse ys ++ xs) []

right, left :: Zipper a -> Zipper a
right (Zip xs (y:ys)) = Zip (y:xs) ys
right z = z
left (Zip (x:xs) ys) = Zip xs (x:ys)
left z = z

afters, befores :: Zipper a -> [a]
afters (Zip _ ys) = ys
befores (Zip xs _) = xs

travel :: Integral i =>  (Zipper a -> Zipper a) -> i -> Zipper a -> Zipper a
travel _ n _ | n < 0 = error "Infinite travel loop"
travel _ 0 z = z
travel f n z = travel f (n-1) (f z)

go :: Integral i => i -> Zipper a -> Zipper a
go i | i >= 0 = travel right i
     | otherwise = travel left $ negate i

insert, push :: a -> Zipper a -> Zipper a
insert a (Zip xs ys) = Zip xs (a:ys)
push a (Zip xs ys) = Zip (a:xs) ys

delete, pop :: Zipper a -> Zipper a
delete (Zip xs (_:ys)) = Zip xs ys
delete z = z
pop (Zip (_:xs) ys) = Zip xs ys
pop z = z

put, replace :: a -> Zipper a -> Zipper a
replace a (Zip xs (_:ys)) = Zip xs (a:ys)
replace _ z = z
put a (Zip xs (_:ys)) = Zip xs (a:ys)
put a (Zip xs []) = Zip xs [a]

change :: (a -> a) -> Zipper a -> Zipper a
change f (Zip xs (y:ys)) = Zip xs (f y:ys)
change _ z = z

-- | Could be dangerous
focus :: Zipper a -> a
focus (Zip _ as) = head as

length :: Zipper a -> Integer
length = foldr (const (+1)) 0

index :: Zipper a -> Int
index (Zip xs _) = Prelude.length xs

-- | A zipper of all available zippers
positions :: Zipper a -> Zipper (Zipper a)
positions z = Zip lefts (z:rights) where
    lefts = unfoldr (\x -> join (,) <$> left' x) z
    rights = unfoldr (\x -> join (,) <$> right' x) z
    left' v = if atStart v then Nothing else Just $ left v
    right' v = if atEnd v then Nothing else Just $ right v

search :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
search check z = find' $ positions z where
    find' (Zip xs ys) = List.find check' (merge xs ys)
    check' (Zip _ []) = False
    check' (Zip _ (y:ys)) = check y
    merge (x:xs) ys = x : merge ys xs
    merge [] ys = ys

find :: Eq a => a -> Zipper a -> Maybe (Zipper a)
find a = search (a ==)
