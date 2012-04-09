{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Format
    ( OutSection(..)
    , Formatter(..)
    , Formattable(..)
    , writeFormat
    , writeSections
    , InSection(..)
    , Loader(..)
    , Loadable(..)
    , readFormat
    , readSections
    , Style(..)
    ) where
import Text.Format.Write
import Text.Format.Read
import Text.Format.Table
