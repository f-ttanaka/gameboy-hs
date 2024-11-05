{-# LANGUAGE GADTs #-}
module Cartridge.Internal where

import Relude

class CartridgeClass a where
  readByte :: MonadIO m => Word16 -> a -> m Word8
  writeByte :: MonadIO m => Word16 -> Word8 -> a -> m ()

data Cartridge where
  Cartridge :: CartridgeClass ca => ca -> Cartridge

instance CartridgeClass Cartridge where
  readByte addr (Cartridge ca) = readByte addr ca
  writeByte addr v (Cartridge ca) = writeByte addr v ca