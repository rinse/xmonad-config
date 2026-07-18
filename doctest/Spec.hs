module Main where

import Test.DocTest


main :: IO ()
main = doctest [ "-ilib", "lib/Lib/Utils.hs" ]
