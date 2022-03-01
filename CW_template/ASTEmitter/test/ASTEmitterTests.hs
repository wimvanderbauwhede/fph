-- You don't have to modify this file but it is OK to add smaller tests for parts of your implementation
module ASTEmitterTests where

import Test.Hspec
import Data.List (intercalate)

import ASTInstances

import AST
import ASTEmitter (
  ppBindings,
  ppAST,
  ppExprTup,
  ppFSig,
  ppArgDecl,
  ppStencilDef   
  )
import ASTParser (
  parseBindings,
  parseMainArgDef,
  parseDecl,
  parseFunDecl,
  parseStencilDef
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe ("\n"++replicate 40 '=' ++"\n" ++ "Provided AST Instance tests ") $ testInstances astInstances
  
testInstances = mapM_ testInstance

testInstance astInstance_ = let
    (instanceName,ast,functionSignaturesList,stencilDefinitionsList,mainArgDeclsList) = astInstance_
  in
    do
      describe ("\n"++replicate 40 '-'++"\nAST Instance test for instance "++instanceName) $ do
        describe "Stencil def pretty-printer tests" $
          mapM_ (\sd -> it (ppStencilDef sd)  (parseStencilDef (ppStencilDef sd) `shouldBe` sd)) stencilDefinitionsList
        describe "Arg decl pretty-printer tests" $
          mapM_ (\ad -> it (ppArgDecl ad)  (parseDecl (ppArgDecl ad) `shouldBe` ad)) (uncurry (++) mainArgDeclsList)
        describe "Fun decl pretty-printer tests" $
          mapM_ (\fd -> it (ppFSig fd)  (parseFunDecl (ppFSig fd) `shouldBe` fd)) functionSignaturesList
        describe "Main bindings pretty-printer test" $ mapM_ (\n ->
          (\aln -> it (ppAST aln !! (n-1)) (parseBindings (ppBindings aln) (instanceState astInstance_) `shouldBe` aln) ) (take n ast)) [1 .. length ast]      
