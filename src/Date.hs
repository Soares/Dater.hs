{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Date where
import Common.Parsec (Parseable, Parser)
-- TODO: Move Common.Parsec (or Utils to Common.Utils)

class ( Num (Delta c)
      , Parseable (Delta c)
      , Show (Delta c)
      ) => Date c where
    data DateTime c :: *
    data Delta c :: *

    parseInEra :: Parser e -> Parser (DateTime c, e)
    showInEra :: String -> DateTime c -> String

    dateTime :: c -> Rational -> DateTime c
    rational :: c -> DateTime c -> Rational

    plus :: c -> Rational -> Delta c -> Rational
    minus :: c -> Rational -> Delta c -> Rational
    clobber :: c -> Rational -> Delta c -> Rational
