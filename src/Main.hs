module Main where

import System.Environment (getArgs)

import Lexer

main :: IO ()
main = do
  code <- getContents
  args <- getArgs
  if ("--parse" `elem` args) then (parseCode code) else (compileCode code)
