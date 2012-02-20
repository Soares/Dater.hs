{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
import Enumable
import Format
import Parse
import Prelude hiding (break)
import TypeLevel.DatePart
import TypeLevel.Naturals

data Year = Year deriving Show
instance Parse Year where parse = undefined
instance Format Year where display = undefined
data Month = Month deriving Show
instance Parse Month where parse = undefined
instance Format Month where display = undefined
data Day = Day deriving Show
instance Parse Day where parse = undefined
instance Format Day where display = undefined

instance DatePart Year Integer (Year, Integer) where
instance DatePart Month (Year, Integer) (Year, Month, Integer) where
instance DatePart Day (Year, Month, Integer) () where

instance Enumable (Year, Integer) where
    zipper y = zMap (const y) (zipper (0 :: Integer))
instance Enumable (Year, Month, Integer) where
    zipper ymd = zMap (const ymd) (zipper (0 :: $(zMod 31)))

v :: (Year :/: Month :/: Day)
v = undefined
mBreak :: DatePart (Year :/: Month :/: Day) Integer () => Integer -> (Year :/: Month :/: Day)
mBreak x = let (a, ()) = break x in a
