{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Format where
import Control.Arrow
import Data.Map (Map)

data Style = Number | Name | Abbreviation Int deriving Show
type Chunk = (Int, Style)

level :: Chunk -> Int
level = fst

style :: Chunk -> Style
style = snd

descend :: Chunk -> Chunk
descend = first $ (-) 1

class Formatter f where
    chunkMap :: f -> Map String Chunk
    renderer :: f -> String -> [Either String Chunk]
    format :: forall d. Format () d => f -> d -> String -> String
    format f d = concatMap (either id $ display () d) . renderer f

class Format x a where
    display :: x -> a -> Chunk -> String
