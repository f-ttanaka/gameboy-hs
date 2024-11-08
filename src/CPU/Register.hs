{-# LANGUAGE TemplateHaskell #-}
module CPU.Register where

import Relude
import Control.Lens
import Data.Bits (Bits, FiniteBits)
import qualified Data.Bits as B

data Registers = Registers
  {
    _regA :: Word8
  , _regB :: Word8
  , _regC :: Word8
  , _regD :: Word8
  , _regE :: Word8
  , _regF :: Word8
  , _regH :: Word8
  , _regL :: Word8
  }

makeLenses ''Registers

-- レジスタを識別する識別子
-- 
data Reg8 = A | B | C | D | E | F | H | L
  deriving (Show, Eq)
data Reg16 = AF | BC | DE | HL
  deriving (Show, Eq)

toReg8Pair :: Reg16 -> (Reg8, Reg8)
toReg8Pair r = case r of
  AF -> (A, F)
  BC -> (B, C)
  DE -> (D, E)
  HL -> (H, L)

getReg8 :: Reg8 -> Registers -> Word8
getReg8 r regs = case r of
  A -> regs ^. regA
  B -> regs ^. regB
  C -> regs ^. regC
  D -> regs ^. regD
  E -> regs ^. regE
  F -> regs ^. regF
  H -> regs ^. regH
  L -> regs ^. regL

setReg8 :: Reg8 -> Word8 -> Registers -> Registers
setReg8 r v regs = case r of
  A -> regs & regA .~ v
  B -> regs & regB .~ v
  C -> regs & regC .~ v
  D -> regs & regD .~ v
  E -> regs & regE .~ v
  F -> regs & regF .~ v
  H -> regs & regH .~ v
  L -> regs & regL .~ v

modifyReg8 :: Reg8 -> (Word8 -> Word8) -> Registers -> Registers
modifyReg8 r f regs = case r of
  A -> regs & regA %~ f
  B -> regs & regB %~ f
  C -> regs & regC %~ f
  D -> regs & regD %~ f
  E -> regs & regE %~ f
  F -> regs & regF %~ f
  H -> regs & regH %~ f
  L -> regs & regL %~ f

getReg16 :: Reg16 -> Registers -> Word16
getReg16 r regs = B.shiftL v1 8 B..|. v2
  where
    (r1, r2) = toReg8Pair r
    fromWord8ToWord16 = fromIntegral :: Word8 -> Word16
    v1 = fromWord8ToWord16 $ getReg8 r1 regs
    v2 = fromWord8ToWord16 $ getReg8 r2 regs

setReg16 :: Reg16 -> Word16 -> Registers -> Registers
setReg16 r v = setReg8 r1 v1 . setReg8 r2 v2
  where
    (r1, r2) = toReg8Pair r
    v1 = fromIntegral $ B.shiftR (v B..&. 0xFF00) 8 :: Word8
    v2 = fromIntegral $ v B..&. 0xFF

-- operations for F register
-- zero | subtract | half carry | carry
data Flag = ZFlag | NFlag | HFlag | CFlag
  deriving (Show, Eq)

flagBit :: Flag -> Int
flagBit f = case f of
  ZFlag -> 7
  NFlag -> 6
  HFlag -> 5
  CFlag -> 4

getFlag :: Flag -> Registers -> Bool
getFlag f regs = B.testBit (regs ^. regF) (flagBit f)

setFlag :: Flag -> Bool -> Registers -> Registers
setFlag f b regs = regs & regF %~ setOrClearBit b (flagBit f)

-- 結果のWord8とキャリーを返す
overflowingAdd :: Word8 -> Word8 -> (Word8, Bool)
overflowingAdd a b = (a+b, a+b < a)

-- return value and flags on addition of two values
-- returned: (result value, carry, half carry, isZero)
addWithFlags :: (Bits a, Num a, Ord a) => a -> a -> (a, Bool, Bool, Bool)
addWithFlags a b =
  ( a+b
  , a+b < a
  , a B..&. 0xF + b B..&. 0xF > 0xF
  , a+b == 0)

-- return value and flags on addition of two values and carry
-- returned: (result value, carry, half carry, isZero)
addCWithFlags :: (Bits a, Num a, Ord a) => a -> a -> Bool -> (a, Bool, Bool, Bool)
addCWithFlags x y c =
  ( n
  , n < x
  , x B..&. 0xF + y B..&. 0xF + vC > 0xF
  , n == 0)
  where
    vC = if c then 1 else 0
    n = x + y + vC


-- オーバーフローしたときに値を循環させる
-- オーバーフローしても実行時エラーなどは起きない
wrappingAdd :: (FiniteBits a, Num a) => a -> a -> a
wrappingAdd x y = x + y B..&. bitMask
  where
    bitSize = B.finiteBitSize x
    bitMask = (1 `B.shiftL` bitSize) - 1

setOrClearBit :: Bits a => Bool -> Int -> a -> a
setOrClearBit test n x = if test then B.setBit x n else B.clearBit x n

halfCarryTest :: (Bits a, Num a, Ord a) => a -> a -> Bool
halfCarryTest v1 v2 = v1 B..&. 0xF + v2 B..&. 0xF > 0xF

modifyRegF :: (Bool, Bool, Bool, Bool) -> Registers -> Registers
modifyRegF (testZero, testSubtract, testHalfCarry, testCarry) =
    modifyReg8 F (setOrClearBit testZero 7)
  . modifyReg8 F (setOrClearBit testSubtract 6)
  . modifyReg8 F (setOrClearBit testHalfCarry 5)
  . modifyReg8 F (setOrClearBit testCarry 4)