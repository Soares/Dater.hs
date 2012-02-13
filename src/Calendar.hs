{-# LANGUAGE TypeFamilies #-}
module Calendar (Calendar(..)) where

class Calendar c where
    data Delta c :: *
    readDelta :: ReadS (Delta c)
    showDelta :: Delta c -> ShowS

    plus :: c -> Rational -> Delta c -> Rational
    minus :: c -> Rational -> Delta c -> Rational
    clobber :: c -> Rational -> Delta c -> Rational

    normalize :: c -> Rational -> Rational
    denormalize :: c -> Rational -> Rational
    display :: c -> Rational -> String
