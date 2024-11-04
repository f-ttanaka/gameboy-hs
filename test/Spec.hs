{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import qualified RegisterSpec (spec) as Reg
import Relude

main :: IO ()
main = hspec $ do
  Reg.spec
