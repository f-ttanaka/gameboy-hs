{-# LANGUAGE GADTs #-}
module CPU.Instruction where

import Relude
import qualified CPU.Register as Reg
import qualified Text.Show

data InstrArg a where
  R8 :: Reg.Reg8 -> InstrArg Word8
  R16 :: Reg.Reg16 -> InstrArg Word16
  R16P :: Reg.Reg16 -> InstrArg Word8 -- address that R16 pointed to. (8 bits)
  N8 :: Int8 -> InstrArg Int8
  N16 :: Int16 -> InstrArg Int16
  SP :: InstrArg Word16
  SPE8 :: Int8 -> InstrArg Word16 -- SP+e8
  HLPI :: InstrArg Word8 -- [HLI], address that pointed by HL and increment HL afterwards.
  HLPD :: InstrArg Word8 -- [HLD] address that pointed by HL and decrement HL afterwards.

-- jump condition
data CC =
    CCNZ -- not zero
  | CCZ  -- zero
  | CCNC -- not carry
  | CCC  -- carry
  | CCAl -- always
  deriving (Show, Eq)

data Instruction =
    ADCA (InstrArg Word8) -- ADC A, x8
  | ADDA (InstrArg Word8) -- ADD A, x8
  | ADDHL (InstrArg Word16) -- ADD HL, x16
  | ADDSP (InstrArg Word16) -- ADD SP, x16
  | ANDA (InstrArg Word8) -- AND A, x8
  | BITU8 Word8 (InstrArg Word8) -- BIT u8, x8 - note: u3 is represented as Word8
  | CALL CC Word16
  | CCF
  | CPA (InstrArg Word8) -- CP A, x8
  | CPL
  | DAA
  | DEC8 (InstrArg Word8)
  | DEC16 (InstrArg Word16)
  | DI
  | EI
  | HALT
  | INC8 (InstrArg Word8)
  | INC16 (InstrArg Word16)
  | JP CC (InstrArg Word16)
  | JR CC Int8
  | LD8 (InstrArg Word8) (InstrArg Word8)
  | LD16 (InstrArg Word16) (InstrArg Word16)
  | LDH (InstrArg Word8) (InstrArg Word8)
  | NOP
  | ORA (InstrArg Word8) -- OR A, x8
  | POP Reg.Reg16
  | PUSH Reg.Reg16
  | RES Word8 (InstrArg Word8) -- RES u8, x8
  | RET CC
  | RETI
  | RL (InstrArg Word8)
  | RLA
  | RLC (InstrArg Word8)
  | RLCA
  | RR (InstrArg Word8)
  | RRA
  | RRC (InstrArg Word8)
  | RRCA
  | RST Word8 -- Word8 represent the order of vec. (order is 3 bit)
  | SBCA (InstrArg Word8) -- SBC A, x8
  | SCF
  | SET Word8 (InstrArg Word8) -- SET u3, x8
  | SLA (InstrArg Word8)
  | SRA (InstrArg Word8)
  | SRL (InstrArg Word8)
  | STOP
  | SUBA (InstrArg Word8) -- SUB A, x8
  | SWAP (InstrArg Word8)
  | XORA (InstrArg Word8) -- XOR A, x8
  deriving Show

instance Show a => Show (InstrArg a) where
  show (R8 r8) = show r8
  show (R16 r16) = show r16
  show _ = "not instanciated for show."

