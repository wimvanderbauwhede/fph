-- Do not change this file
module Main where

import ASTEmitter (ppProgram)
import ASTInstances (astInstances)

main :: IO ()
main = ppProgram (last astInstances)
