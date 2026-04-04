module Main where

import System.Environment (getArgs)

import Codegen (parseCodePrint, compileCodePrint)

main :: IO ()
main = do
  code <- getContents
  args <- getArgs
  if ("--parse" `elem` args) then (parseCodePrint code) else (compileCodePrint code)
