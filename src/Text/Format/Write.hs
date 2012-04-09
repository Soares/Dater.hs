module Text.Format.Write where

writeFormat :: Format f, Formatter x => [Either (Section f) String] -> x -> String
