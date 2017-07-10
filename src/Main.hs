module Main where

import Esp.Lexer (testLexer)

main :: IO ()
main = getContents >>= testLexer
