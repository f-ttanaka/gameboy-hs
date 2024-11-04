{-# LANGUAGE GADTs #-}
module Bus where

import Relude
import Cartridge.Internal (Cartridge, readByte)

newtype Bus = Bus
  { cartridge :: Cartridge}

readAddress :: Word16 -> Bus -> Word8
readAddress addr bus = readByte addr (cartridge bus)