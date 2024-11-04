{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module CPU.CPU where

import Relude
import Bus (Bus)
import Control.Lens
import CPU.Register

-- CPU
-- CPUM: CPU の可変な状態をまとめたもの
-- CPUIM: 不変な状態をまとめたもの
data CPUM = CPUM 
  {
    _registers :: Registers
  , _programCounter :: Word16
  , _stackPointer :: Word16
  }

makeLenses ''CPUM

newtype CPUIM = CPUIM
  {
    bus :: Bus
  }

getRegisters :: MonadState CPUM m => m Registers
getRegisters = use registers

getSP :: MonadState CPUM m => m Word16
getSP = use stackPointer

askBus :: MonadReader CPUIM m => m Bus
askBus = bus <$> ask

-- fromByte :: MonadThrow m => Bool -> Word8 -> m Instruction
-- fromByte prefixed r = case r of
--   0x00 | prefixed -> return $ RLC B
--   0x02 | not prefixed -> return $ INC (B,C)
--   _ -> throwM $ NotOperand r
