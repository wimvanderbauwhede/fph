{- 
Complete the Expr datatype 
You only have to complete the four lines with "..." (lines 68 .. 71). 
Leave everything else as it is. 
-}

{-# LANGUAGE DeriveDataTypeable #-}
module AST where

import Data.Generics (Data, Typeable)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
(!) :: Ord k => Map.Map k a -> k -> a
(!) = (Map.!)

{-# ANN module "HLint: ignore Use zipWith" #-}
{-# ANN module "HLint: ignore Use second" #-}
{-# ANN module "HLint: ignore Use uncurry" #-}

type Name = String
type Size = Int
type Offset = Int

-- Vector:In   Out  Stencil Temp Don't care
data VE = VI  | VO  | VS  | VT | VDC | VU deriving (Show, Read, Ord, Eq, Data, Typeable)
data DType =
    DInteger | DInt
  | DReal | DFloat
  | DSVec Int DType --  to encode SVecs
  | DVec Int DType --  to encode Vecs
  | DFVec [(Int,Int)] DType --  to encode FVecs
  | DTuple [DType] -- to encode Tuple
  | DDC -- Don't Care ; Int and Integer, Real and Float as I can't make up my mind
    deriving (Show, Read, Ord, Eq, Data, Typeable)

type AST = [(Expr,Expr)]
type FunctionSignature = (String,[Expr])
type StencilDefinition = (String,[Integer])
type MainArgDecls = ([(String,DType)],[(String,DType)])

type ASTInstance = (String, AST, [FunctionSignature], [StencilDefinition], MainArgDecls)

data ASTInstanceState = ASTInstanceState {
  stencils :: Map.Map String Int,
  variables :: Map.Map String (DType,VE),
  functions :: Map.Map String [Expr]
  } deriving (Show)

instanceState  ( _, ast1, functionSignaturesList, stencilDefinitionsList, mainArgDeclsList) = let
    stencils_ = Map.fromList $ map (\(sn,sd) -> (sn,length sd)) stencilDefinitionsList
    variables_ = Map.fromList $ map (\(vn,vt) -> (vn,(vt,VI))) (fst mainArgDeclsList) ++ map (\(vn,vt) -> (vn,(vt,VO))) (snd mainArgDeclsList)
    functions_ = Map.fromList $ functionSignaturesList
  in
    ASTInstanceState stencils_ variables_ functions_

type DeclType = Expr

data Expr =
        -- Primitive types:
                      Scalar VE DType Name
                    | Const Int
                    | Tuple [Expr]
                    | Vec VE Expr -- Name -- (Vec ve (SVec sz (Scalar ve dt v_n) ) )
                    | SVec Size Expr -- Name
                    | FVec [(Offset,Size)] Expr -- This is for non-map args FIXME! bounds i.o range
                    | Function Name [Expr] -- 2nd arg is list of non-map/fold args
        -- Higher-order types: add the types for zipt, unzipt, map and stencil
                    | ... -- zipt
                    | ... -- unzipt
                    | ... -- map f v
                    | ... -- stencil s v
                        deriving (Show, Read, Ord, Data, Typeable)

instance Eq Expr where
  (==) (Scalar _ dt1 n1) (Scalar _ dt2 n2) = (dt1==dt2) || (n1==n2)
  (==) (Const i1) (Const i2)  = i1 == i2
  (==) (Tuple es1) (Tuple es2)  = listEq es1 es2
  (==) (SVec i1 e1) (SVec i2 e2) = (i1==i2) && (e1==e2)
  (==) (FVec l1 e1) (FVec l2 e2) = (l1==l2) && (e1==e2)
  (==) (Vec i1 e1) (Vec i2 e2) = (i1==i2) && (e1==e2)
  (==) (ZipT es1) (ZipT es2) = listEq es1 es2
  (==) (UnzipT e1) (UnzipT e2) = e1==e2
  (==) (Map f1 v1) (Map f2 v2) = (f1 == f2) && (v1 == v2)
  (==) (Function n1 es1) (Function n2 es2) = (n1==n2) && listEq es1 es2
  (==) (Stencil s1 v1) (Stencil s2 v2) = (s1 == s2) && (v1 == v2)
  (==) _ _ = False

listEq l1 l2 =  foldl' (&&) True (map (uncurry (==)) (zip l1 l2))

sigTypes :: MainArgDecls -> ([DType],[DType])
sigTypes mainArgDecls = let
    (in_tts,out_tts) = mainArgDecls
    [in_tts',out_tts'] = map (map snd) [in_tts,out_tts]
  in
    (in_tts',out_tts')

argNames :: MainArgDecls -> ([String],[String])
argNames mainArgDecls = let
    (in_tts,out_tts) = mainArgDecls
    [in_tts',out_tts'] = map (map fst) [in_tts,out_tts]
  in
    (in_tts',out_tts')