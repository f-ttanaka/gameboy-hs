module CPU.Error where

import Relude
import qualified Text.Show

data ExecError =
    DefaultError String
  | NotOperand Word8
instance Exception ExecError

instance Show ExecError where
  show (DefaultError err) = err
  show (NotOperand w8) = show w8 ++ " is not a instruction."