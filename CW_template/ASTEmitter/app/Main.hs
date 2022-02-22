-- Do not change this file
module Main where

import ASTEmitter (ppProgram)
import ASTInstance (astInstance)

main :: IO ()
main = ppProgram astInstance
