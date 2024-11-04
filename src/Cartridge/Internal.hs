{-# LANGUAGE GADTs #-}
module Cartridge.Internal where

import Relude

class CartridgeClass a where
  readByte :: Word16 -> a -> Word8

data Cartridge where
  Cartridge :: CartridgeClass ca => ca -> Cartridge

instance CartridgeClass Cartridge where
  readByte addr (Cartridge ca) = readByte addr ca