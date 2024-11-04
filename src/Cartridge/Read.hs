module Cartridge.Read where

import Relude

import qualified Data.ByteString as B
import Text.Printf ( printf )

printHex :: B.ByteString -> IO ()
printHex bs = B.foldr (\w rest -> printf "%02X " w >> rest) (putStrLn "") bs

readCartridge :: IO ()
readCartridge = do
  contents <- B.readFile "resource/test-rom/cpu_instrs/01-special.gb"
  printHex contents