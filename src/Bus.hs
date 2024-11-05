{-# LANGUAGE GADTs #-}
module Bus where

import Relude
import Cartridge.Internal (Cartridge, readByte, writeByte)

newtype Bus = Bus
  { cartridge :: Cartridge}

readAddress :: MonadIO m =>  Word16 -> Bus -> m Word8
readAddress addr bus = readByte addr (cartridge bus)

writeAddress :: MonadIO m =>  Word16 -> Word8 -> Bus -> m ()
writeAddress addr v bus = writeByte addr v (cartridge bus)