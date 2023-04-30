{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import PlutusTx.Prelude
import PlutusTx.Builtins
import Data.Maybe              (fromJust)
import Data.Text               (pack)
import Prelude                 (String)
import Text.Hex                (decodeHex)


-- Helper Functions

hexConvert :: String -> BuiltinByteString
hexConvert = toBuiltin . fromJust . decodeHex . pack

