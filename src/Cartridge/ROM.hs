module Cartridge.ROM where

import Relude
import Cartridge.Internal (CartridgeClass)

newtype ROM = ROM {romBytes :: ByteString}
instance CartridgeClass ROM