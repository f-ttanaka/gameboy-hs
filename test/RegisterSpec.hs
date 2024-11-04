module RegisterSpec (spec) where

import Relude
import Test.Hspec
import Core.Register

mockRegister :: IO Registers
mockRegister = Registers
  <$> newIORef (0xFF :: Word8)
  <*> newIORef (0xFF :: Word8)
  <*> newIORef (0xFF :: Word8)
  <*> newIORef (0xFF :: Word8)
  <*> newIORef (0xFF :: Word8)
  <*> newIORef (0xFF :: Word8)
  <*> newIORef (0xFF :: Word8)
  <*> newIORef (0xFF :: Word8)

spec :: Spec
spec = do
  describe "getReg8" $ do
    it "case1" $
      (flip getReg8 A =<< mockRegister) `shouldReturn` 0xFF
