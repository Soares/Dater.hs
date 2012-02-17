{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Calendar (Calendar(..)) where

class (Show c, Read (Delta c), Show (Delta c)) => Calendar c where
    data Delta c :: *

    display :: c -> Rational -> String
    parse :: c -> ReadS Rational

    plus :: c -> Rational -> Delta c -> Rational
    minus :: c -> Rational -> Delta c -> Rational
    clobber :: c -> Rational -> Delta c -> Rational

    beginning :: c -> Rational

    normalize :: c -> Rational -> Rational
    normalize c r = r + beginning c

    denormalize :: c -> Rational -> Rational
    denormalize c r = r - beginning c
