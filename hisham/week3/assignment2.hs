module Assignment2 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

--Assignment 2: Time: 3 hours
-- example use: parseTest form1
parseTest :: Form -> Bool
parseTest f = (parse . show) f == [f]
