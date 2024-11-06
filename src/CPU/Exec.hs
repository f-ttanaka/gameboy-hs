{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module CPU.Exec
  ( execute ) where

import Relude
import qualified Bus
import Control.Exception.Safe (MonadThrow, throwString)
import Control.Lens
import CPU.CPU
import CPU.Instruction
import CPU.Register

-- type Check = Either SomeException
newtype Exec a = Exec (ReaderT CPUIM (StateT CPUM IO) a)
  deriving
    (Functor
    , Applicative
    , Monad
    , MonadReader CPUIM
    , MonadState CPUM
    , MonadThrow
    , MonadIO)

readValue :: InstrArg a -> Exec a
readValue (R8 r) = getReg8 r <$> getRegisters
readValue (R16 r) = getReg16 r <$> getRegisters
readValue (R16P r) = do
  addr <- getReg16 r <$> getRegisters
  Bus.readAddress addr =<< askBus
readValue (N8 n) = return n
readValue (N16 n) = return n
readValue SP = getSP
readValue (SPE8 n) = do
  sp <- getSP
  let sp' = fromIntegral sp :: Int8
  return $ fromIntegral $ sp' + n
readValue HLPI = do
  addr <- getReg16 HL <$> getRegisters
  registers %= setReg16 HL (succ addr)
  Bus.readAddress addr =<< askBus
readValue HLPD = do
  addr <- getReg16 HL <$> getRegisters
  registers %= setReg16 HL (pred addr)
  Bus.readAddress addr =<< askBus

(<--) :: InstrArg a -> a -> Exec ()
(R8 r) <-- v = registers %= setReg8 r v
(R16 r) <-- v = registers %= setReg16 r v
(R16P r) <-- v = do
  addr <- getReg16 r <$> getRegisters
  Bus.writeAddress addr v =<< askBus
(N8 _) <-- _ = throwString "invalid argument N8."
(N16 _) <-- _ = throwString "invalid argument N16."
SP <-- v = stackPointer .= v
(SPE8 _) <-- _ = throwString "invalid argument SPE8."
HLPI <-- v = do
  addr <- getReg16 HL <$> getRegisters
  registers %= setReg16 HL (succ addr)
  Bus.writeAddress addr v =<< askBus
HLPD <-- v = do
  addr <- getReg16 HL <$> getRegisters
  registers %= setReg16 HL (pred addr)
  Bus.writeAddress addr v =<< askBus

conditionTest :: CC -> Registers -> Bool
conditionTest cc regs = case cc of
  CCZ -> getFlag ZFlag regs
  CCNZ -> not $ getFlag ZFlag regs
  CCC -> getFlag CFlag regs
  CCNC -> not $ getFlag CFlag regs
  CCAl -> True

-- 次のプログラムカウンタへの移動量を返す
execute :: Instruction -> Exec Word16
execute ins = case ins of
  ADCA arg -> do
    v <- readValue arg
    vA <- getReg8 A <$> getRegisters
    c <- getFlag CFlag <$> getRegisters
    let (res, carry, hCarry, isZero) = addCWithFlags vA v c
    registers %= setReg8 A res
    registers %= setFlag ZFlag isZero
    registers %= setFlag NFlag False
    registers %= setFlag HFlag hCarry
    registers %= setFlag CFlag carry
    return 1
  ADDA arg -> do
    v <- readValue arg
    vA <- getReg8 A <$> getRegisters
    let (res, carry, hCarry, isZero) = addWithFlags vA v
    registers %= setReg8 A res
    registers %= setFlag ZFlag isZero
    registers %= setFlag NFlag False
    registers %= setFlag HFlag hCarry
    registers %= setFlag CFlag carry
    return 1
  ADDHL arg -> do
    v <- readValue arg
    vHL <- readValue (R16 HL) -- HL
    let (res, carry, hCarry, _) = addWithFlags vHL v
    registers %= setReg16 HL res
    registers %= setFlag NFlag False
    registers %= setFlag HFlag hCarry
    registers %= setFlag CFlag carry
    return 1
  ADDSP arg -> do
    v <- readValue arg
    vSP <- getSP
    let (res, carry, hCarry, _) = addWithFlags vSP v
    stackPointer .= res
    registers %= setFlag ZFlag False
    registers %= setFlag NFlag False
    registers %= setFlag HFlag hCarry
    registers %= setFlag CFlag carry
    return 1
  _ -> throwString $ show ins ++ " is not implemented instruction."
