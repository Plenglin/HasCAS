module TestUtils where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Ratio
import qualified Data.Map as Map

import Lib


w = exprv "w"
x = exprv "x"
y = exprv "y"
z = exprv "z"
