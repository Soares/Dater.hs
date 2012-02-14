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

    normalize :: c -> Rational -> Rational
    denormalize :: c -> Rational -> Rational
