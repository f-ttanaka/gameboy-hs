{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module CPU.Exec where

import Relude
import Control.Exception.Safe (MonadThrow, throwM)
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
readValue (R16P r) = undefined
readValue (N8 n) = return n
readValue (N16 n) = return n
readValue SP = getSP
readValue (SPE8 n) = do
  sp <- getSP
  let sp' = fromIntegral sp :: Int8
  return $ fromIntegral $ sp' + n

-- addValue :: Word8 -> Exec Word8
-- addValue v = do
--   cpu <- get
--   let vA = getReg8 (registers cpu) A
--       (nv, carry) = overflowingAdd vA v
--   -- Fレジスタの操作
--   -- zero flag
--   put $ cpu {registers = 
--               modifyRegF (nv == 0, False, halfCarryTest vA v, carry) (registers cpu)}
--   return nv

-- 次のプログラムカウンタへの移動量を返す
-- execute :: Instruction -> Exec Word16
-- execute ins = do
--   cpu <- get
--   case ins of
--     ADD C -> do
--       let v = getReg8 (registers cpu) C
--       nv <- addValue v
--       put $ cpu {registers = setReg8 A nv (registers cpu)}
--       return 1
--     _ -> throwM . DefaultError $ show ins ++ " is not implemented instruction."
   