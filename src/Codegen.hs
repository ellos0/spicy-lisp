module Codegen (showC, parseCodePrint, compileCodePrint) where

import Struct
import Text.ParserCombinators.Parsec (parse)
import Lexer
import System.IO


compileCode :: String -> String
compileCode code = case parse parseExpr "lisp" code of
                 Right val -> (showC val)
                 Left _ -> ""

parseCodePrint :: String -> IO ()
parseCodePrint code = do
  case parse parseExpr "lisp" code of
    Right val -> (putStrLn (show val))
    Left err -> (hPutStrLn stderr ("Error: " ++ show err))

compileCodePrint :: String -> IO ()
compileCodePrint code = do
  case parse parseExpr "lisp" code of
    Right val -> (putStrLn (showC val))
    Left err -> (hPutStrLn stderr ("Error: " ++ show err))


includeFile :: FilePath -> IO String
includeFile file = do
            contents <- readFile file
            return $ compileCode contents

makeOp :: String -> [LispVal] -> String
makeOp op args = concat [showC (args !! 0), " ", op, " ", showC (args !! 1)]

showC :: LispVal -> String
showC (Atom x) = x
showC (Number x) = show x
showC (String x) = "\"" ++ x ++ "\""
showC (Bool x) = if x then "true" else "false"
showC (List []) = ""
showC (List (op:args)) = case (op) of
  (Atom "+") -> makeOp "+" args
  (Atom "-") -> makeOp "-" args
  (Atom "*") -> makeOp "*" args
  (Atom "/") -> makeOp "/" args

  (Atom "add") -> makeOp "+" args
  (Atom "subtract") -> makeOp "-" args
  (Atom "multiply") -> makeOp "*" args
  (Atom "divide") -> makeOp "/" args

  (Atom "and") -> makeOp "&&" args
  (Atom "or") -> makeOp "||" args
  (Atom "not") -> makeOp "!" args
  
  (Atom "print") -> concat ["printf(", showC (args !! 0), ");"]
  (Atom "defun") -> concat [showC (args !! 1), " ", showC (args !! 0), "(){\n", showC (args !! 2), "\n}\n"]
  (Atom "type") -> (case (args !! 0) of
                      (Atom "i4") -> "int"
                      (Atom "i2") -> "short"
                      (Atom "bool") -> "bool"
                      )

  (Atom "if") -> concat ["if (", showC (args !! 0), ") {", showC (args !! 1), "}"]

  -- (Atom "include") -> includeFile (showC $ args !! 0)
  
  (List _) -> concat (map showC (op:args))
  _ -> ""
