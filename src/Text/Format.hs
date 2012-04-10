{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Format
    ( OutSection(..)
    , Formatter(..)
    , Formattable(..)
    , writeFormatIn
    , writeFormat
    , writeSpecIn
    , writeSpec
    , InSection(..)
    , Loader(..)
    , Loadable(..)
    , readFormatIn
    , readFormat
    , readSpecIn
    , readSpec
    , Style(..)
    ) where
import Text.Format.Write
import Text.Format.Read
import Text.Format.Table
