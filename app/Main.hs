{-# LANGUAGE LambdaCase #-}
module Main where

import Lib
import Euterpea
import System.Environment (getArgs)

main :: IO ()
main = do
  getArgs >>= \case
    (file : []) -> writeMidi file adrenalina
    _ -> error "Just give a single argument as a file name"
