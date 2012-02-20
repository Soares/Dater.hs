module Data.List.Zipper where

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
        (backwards (zipWith ($) fs) xs) (zipWith ($) gs xs)
instance Foldable Zipper where
    foldr f z (Zip xs ys) = foldl (flip f) (foldr f z ys) xs
instance Traversable Zipper where
    traverse f (Zip xs ys) = Zipper <$>
        (reverse <$> traverse f $ reverse xs) <*> traverse f ys

empty :: Zipper a
empty = Zip [] []

fromList, fromListEnd :: [a] -> Zipper a
fromList as = Zip [] as
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

travel :: Integral i =>  (Zipper a -> Zipper a) -> i -> Zipper a -> Zipper a
travel _ n _ | n < 0 = error "Infinite travel loop"
travel _ 0 z = z
travel f n z = travel f (n-1) (f z)

insert, push :: a -> Zipper a -> Zipper a
insert a (Zip xs ys) = Zip xs (a:ys)
push a (Zip xs ys) = Zip (a:xs) ys

delete, pop :: Zipper a -> Zipper a
delete (Zip xs (y:ys)) = Zip xs ys
delete z = z
pop (Zip (x:xs) ys) = Zip xs ys
pop z = z

put, replace :: a -> Zipper a -> Zipper a
replace a (Zip xs (_:ys)) = Zip xs (a:ys)
replace z = z
put a (Zip xs (_:ys)) = Zip xs (a:ys)
put a (Zip xs []) = Zip xs [a]

change :: (a -> a) -> Zipper a -> Zipper a
change f (Zip xs (y:ys)) = Zip xs (f y:ys)
change _ z = z
