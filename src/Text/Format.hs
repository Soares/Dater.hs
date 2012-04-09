{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Format
    ( OutSection
    , Formatter(..)
    , Formattable(..)
    , writeFormat
    , writeSections
    , InSection
    , Loader(..)
    , Loadable(..)
    , readFormat
    , readSections
    ) where
import Text.Format.Write
import Text.Format.Read
