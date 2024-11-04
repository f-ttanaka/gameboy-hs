{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module CPU.CPU where

import Relude
import Control.Exception.Safe (MonadThrow, throwM)
import CPU.Error
import CPU.Instruction
import CPU.Register
import qualified Data.Bits as B
import Data.Vector (Vector)
import qualified Data.Vector as V

-- CPU
-- CPUM: CPU の可変な状態をまとめたもの
-- CPUIM: 不変な状態をまとめたもの
data CPUM = CPUM 
  {
    registers :: Registers
  , programCounter :: Word16
  , stackPointer :: Word16
  }

newtype CPUIM = CPUIM
  {
    bus :: Vector Word8
  }

getRegisters :: MonadState CPUM m => m Registers
getRegisters = registers <$> get

getSP :: MonadState CPUM m => m Word16
getSP = stackPointer <$> get

readByte :: CPUIM -> Word16 -> Word8
readByte mb addr = bus mb V.! B.finiteBitSize addr

-- fromByte :: MonadThrow m => Bool -> Word8 -> m Instruction
-- fromByte prefixed r = case r of
--   0x00 | prefixed -> return $ RLC B
--   0x02 | not prefixed -> return $ INC (B,C)
--   _ -> throwM $ NotOperand r
