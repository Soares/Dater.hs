{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sized
    ( Sized(..)
    , SizedIn(..)
    , sizeFrom
    ) where
import Zeroed
import Range

class Zeroed a => Sized a where
    size :: a -> Integer

class SizedIn a x where
    sizeIn :: x -> a -> Integer

-- Helper function to 'auto-derive' size
-- The first argument specifies the type to derive from,
-- and will never be used. 'undefined' is a valid value.
--
-- > newtype Year = Y Integer
-- > newtype Month = M Integer
-- > instance Ranged Month Year where
-- >     start = const 1
-- >     end = const 12
-- > instance SizedIn Month Year where
-- >     sizeIn y 2 = if y `mod` 4 /= 0 then 28 else 29
-- >     sizeIn _ n = if n `elem` [9,4,6,9] then 30 else 31
-- > instance Sized Year where
-- >     size = sizeFrom (undefined :: Month)
--
-- Years above have size 365 or 366 depending upon year `mod` 4
-- (Note: actual leap year calculation is a bit more complex)
sizeFrom :: forall a b. (Ranged b a, SizedIn b a) => b -> a -> Integer
sizeFrom _ a = sum $ map (sizeIn a) (elements a :: [b])


-- Idiot instances
instance Sized Integer where size = id
instance Sized Int where size = fromIntegral
