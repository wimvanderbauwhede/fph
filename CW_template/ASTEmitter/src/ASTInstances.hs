-- In this source file you can add additional test cases.
-- Create them analogous to the examples below and put them in the astInstances list
module ASTInstances ( ast
        , functionSignaturesList
        , stencilDefinitionsList
        , mainArgDeclsList 
        , astInstance
        , astInstances
        ) where

import AST

astInstances :: [ASTInstance]
astInstances = [astInstance1,astInstance2,astInstance3,astInstance4,astInstance]

-- ============================================================================================================================================
-- Test cases:
{-
Test case 1:

v_in :: Vec 16 Float
v_out :: Vec 16 Float
main :: Vec 16 Float -> Vec 16 Float
main v_in = let
  v_out = v_in
in 
  v_out

-}
ast1 = [
    (
      Vec VO (Scalar VDC DFloat "v_out"),     
      Vec VI (Scalar VDC DFloat "v_in")
    )
  ]
  
functionSignaturesList1 = []
stencilDefinitionsList1 = []
mainArgDeclsList1 = (
    [("v_in" , DVec 16 DFloat )],
    [("v_out" , DVec 16 DFloat )]
  )

astInstance1 = (
  "Test 1: no function",
  ast1,
  functionSignaturesList1,
  stencilDefinitionsList1,
  mainArgDeclsList1)
-- ============================================================================================================================================
{-
Test case 2:  

v_in :: Vec 16 Float
v_out :: Vec 16 Float
f :: Float -> Float
main :: Vec 16 Float -> Vec 16 Float
main v_in = let
  v_out = map f v_in
in 
  v_out
-}

ast2 = [
    (Vec VO (Scalar VDC DFloat "v_out"), 
    Map 
      (Function "f"  []) 
      (Vec VI (Scalar VDC DFloat "v_in"))
    )
  ]

functionSignaturesList2 = [
    ("f",  [
        Tuple [],
        Scalar VT DFloat "w_1",
        Scalar VO DFloat "w_3"
      ]
    )
  ]
stencilDefinitionsList2 = []
mainArgDeclsList2 = (
    [("v_in" , DVec 16 DFloat )],
    [("v_out" , DVec 16 DFloat )]
  )  
  
astInstance2 = (
  "Test 2: single function of scalars",
  ast2,
  functionSignaturesList2,
  stencilDefinitionsList2,
  mainArgDeclsList2)
-- ============================================================================================================================================
{-
Test case 3:  

u_0 :: Vec 16 Float
v_0 :: Vec 16 Float
u_1 :: Vec 16 Float
v_3 :: Vec 16 Float
f_1 :: (Float,Float) -> Float
f_2 :: (Float,Float) -> Float
main :: (Vec 16 Float,Vec 16 Float) -> Vec 16 Float
main (u_0,v_0) = let
  u_1 = map f_1 (zipt (u_0,v_0))
  v_3 = map f_2 (zipt (u_1,v_0))
in 
  (u_1,v_3)
-}
  
ast3 =   [
        ( Vec VO (Scalar VDC DFloat "u_1"), 
          Map 
            (Function "f_1"  []) 
            (ZipT [
              Vec VI (Scalar VDC DFloat "u_0"),
              Vec VI (Scalar VDC DFloat "v_0")              
              ]) 
          )
        ,(Vec VO (Scalar VDC DFloat "v_3"),
          Map 
            (Function "f_2"  []) 
            (ZipT [
              Vec VO (Scalar VDC DFloat "u_1"),
              Vec VI (Scalar VDC DFloat "v_0")
              ]) 
          ) 
  ]

functionSignaturesList3 = [
    ("f_1",  [
        Tuple [],
        Tuple [Scalar VI DFloat "u_0",Scalar VI DFloat "v_0"],
        Scalar VT DFloat "u_1"
        
      ]
    ),
    ("f_2",  [
        Tuple [],
        Tuple [Scalar VO DFloat "u_1",Scalar VI DFloat "v_0"],
        Scalar VO DFloat "v_3"        
      ]
    ) 
  ]
stencilDefinitionsList3 = []
mainArgDeclsList3 = (
    [("u_0" , DVec 16 DFloat ),("v_0" , DVec 16 DFloat )],
    [("u_1" , DVec 16 DFloat ),("v_3" , DVec 16 DFloat )]    
  )    
  
astInstance3 = (
  "Test 3: two functions of tuples",
  ast3,
  functionSignaturesList3,
  stencilDefinitionsList3,
  mainArgDeclsList3)
-- ============================================================================================================================================
{-
Test case 4:  

u_0 :: Vec 16 Float
v_0 :: Vec 16 Float
u_2 :: Vec 16 Float
v_4 :: Vec 16 Float
fv_1 :: FVec [(0,7)] Float
fv_2 :: FVec [(0,3)] Float
f_1 :: (Float,Float) -> Float
f_2 :: (FVec [(0,7)] Float,FVec [(0,3)] Float) -> (Float,Float) -> (Float,Float)
f_3 :: (Float,Float) -> Float
main :: (FVec [(0,7)] Float,FVec [(0,3)] Float,Vec 16 Float,Vec 16 Float) -> Vec 16 Float
main (fv_1,fv_2,u_0,v_0) = let
  u_1 = map f_1 (zipt (u_0,v_0))
  (u_2,v_3) = unzipt (map (f_2 (fv_1,fv_2)) (zipt (v_0,u_1)))
  v_4 = map f_3 (zipt (v_3,u_0))
in 
  (u_2,v_4)  

-}
ast4 =   [
        ( Vec VT (Scalar VDC DFloat "u_1"), 
          Map 
            (Function "f_1"  []) 
            (ZipT [
              Vec VI (Scalar VDC DFloat "u_0"),
              Vec VI (Scalar VDC DFloat "v_0")              
              ]) 
          )
        ,(Tuple [Vec VO (Scalar VDC DFloat "u_2"),Vec VT (Scalar VDC DFloat "v_3")],
          UnzipT (
          Map 
            (Function "f_2"  [FVec [(0,7)] (Scalar VDC DFloat "fv_1"),FVec [(0,3)] (Scalar VDC DFloat "fv_2")]) 
            (ZipT [
              Vec VI (Scalar VDC DFloat "v_0"),
              Vec VT (Scalar VDC DFloat "u_1")
              ]) 
          ) 
        ),
        ( Vec VO (Scalar VDC DFloat "v_4"), 
          Map 
            (Function "f_3"  []) 
            (ZipT [
              Vec VT (Scalar VDC DFloat "v_3"),
              Vec VI (Scalar VDC DFloat "u_0")              
              ]) 
          )        
  ]

functionSignaturesList4 = [
    ("f_1",  [
      Tuple [],
      Scalar VT DFloat "w_1",
      Tuple [Scalar VO DFloat "w_3",Scalar VO DFloat "w_3"]
    ]
    ),
    ("f_2",  [
        Tuple [],
        Scalar VT DFloat "w_1",
        Tuple [Scalar VO DFloat "w_3",Scalar VO DFloat "w_3"]
      ]
    ),
    ("f_3",  [
        Tuple [],
        Scalar VT DFloat "w_1",
        Tuple [Scalar VO DFloat "w_3",Scalar VO DFloat "w_3"]
      ]
    ) 
  ]
stencilDefinitionsList4 = []
mainArgDeclsList4 = (
    [("fv_1" , DFVec [(0,7)] DFloat ),
    ("fv_2" , DFVec [(0,3)] DFloat  ),  
    ("u_0" , DVec 16 DFloat ),
    ("v_0" , DVec 16 DFloat )
    ],
    [("u_2" , DVec 16 DFloat ),("v_4" , DVec 16 DFloat )]
  )    
astInstance4 = ( 
  "Test 4: 3 functions of tuples, one with non-map args",
  ast4,
  functionSignaturesList4,
  stencilDefinitionsList4,
  mainArgDeclsList4)
-- ============================================================================================================================================
{-
Test case 5: 

-- Stencil declarations
s1 = [305,23409,23410,23560,23561]
s2 = [23561,23562,23713,46817]
s3 = [305,23409,23560,23561,23712]
s4 = [23409,46665]
s5 = [46665,46816,46817]
s6 = [23560,46816]

-- Argument type declarations
-- Input arguments
v_0 :: Vec 2139552 Float
w_0 :: Vec 2139552 Float
u_0 :: Vec 2139552 Float

dx1_0 :: FVec [(-1,151)] Float
dy1_0 :: FVec [(0,151)] Float
dz1_0 :: FVec [(-1,92)] Float

-- Output arguments
v_3 :: Vec 2074891 Float
w_3 :: Vec 2074891 Float

-- Function type declarations
f_1 :: (FVec [(-1,151)] Float,FVec [(0,151)] Float,FVec [(-1,92)] Float) -> (SVec 5 Float,SVec 5 Float,SVec 4 Float) -> (Float,Float,Float)
f_2 :: (FVec [(-1,151)] Float,FVec [(0,151)] Float) -> (SVec 2 Float,SVec 3 Float,SVec 2 Float) -> Float
f_3 :: Float -> Float

main :: (Vec 2139552 Float,Vec 2139552 Float,Vec 2139552 Float,FVec [(-1,151)] Float,FVec [(0,151)] Float,FVec [(-1,92)] Float) -> (Vec 2074891 Float,Vec 2074891 Float)
main (v_0,w_0,u_0,dx1_0,dy1_0,dz1_0) = let
    -- f_1
    v_s_0 = stencil s1 v_0
    w_s_0 = stencil s2 w_0
    u_s_0 = stencil s3 u_0
    (u_1,v_1,w_1) = unzipt (map (f_1 (dx1_0,dy1_0,dz1_0)) (zipt (u_s_0,v_s_0,w_s_0)))
    -- f_2
    v_s_1 = stencil s4 v_1
    w_s_1 = stencil s5 w_1
    u_s_1 = stencil s6 u_1
    v_3 = map (f_2 (dx1_0,dy1_0)) (zipt (u_s_1,w_s_1,v_s_1))
    -- f_3
    w_3 = map f_3 w_1
  in
    (v_3,w_3)
-}

ast :: AST
ast = [
       (Vec VS (SVec 5 (Scalar VDC DFloat "v_s_0" )) , Stencil (SVec 5 (Scalar VDC DInt "s1")) (Vec VI (Scalar VDC DFloat "v_0")))
       ,(Vec VS (SVec 4 (Scalar VDC DFloat "w_s_0" )) , Stencil (SVec 4 (Scalar VDC DInt "s2")) (Vec VI (Scalar VDC DFloat "w_0")))
       ,(Vec VS (SVec 5 (Scalar VDC DFloat "u_s_0" )) , Stencil (SVec 5 (Scalar VDC DInt "s3")) (Vec VI (Scalar VDC DFloat "u_0")))
       ,( Tuple [Vec VT (Scalar VDC DFloat "u_1"),Vec VT (Scalar VDC DFloat "v_1"),Vec VT (Scalar VDC DFloat "w_1")], 
          UnzipT ( Map 
            (Function "f_1"  [FVec [(-1,92)] (Scalar VI DFloat "dz1_0"),FVec [(-1,151)] (Scalar VI DFloat "dx1_0"),FVec [(0,151)] (Scalar VI DFloat "dy1_0")]) 
            (ZipT [
              Vec VS (SVec 5 (Scalar VDC DFloat "u_s_0")),
              Vec VS (SVec 5 (Scalar VDC DFloat "v_s_0")),
              Vec VS (SVec 4 (Scalar VDC DFloat "w_s_0"))
              ]) )
          )
       ,(Vec VS (SVec 2 (Scalar VDC DFloat "v_s_1" )) , Stencil (SVec 2 (Scalar VDC DInt "s4")) (Vec VT (Scalar VDC DFloat "v_1")))
       ,(Vec VS (SVec 3 (Scalar VDC DFloat "w_s_1" )) , Stencil (SVec 3 (Scalar VDC DInt "s5")) (Vec VT (Scalar VDC DFloat "w_1")))
       ,(Vec VS (SVec 2 (Scalar VDC DFloat "u_s_1" )) , Stencil (SVec 2 (Scalar VDC DInt "s6")) (Vec VT (Scalar VDC DFloat "u_1")))
       ,(Vec VO (Scalar VDC DFloat "v_3"),
         Map 
            (Function "f_2"  [FVec [(-1,151)] (Scalar VI DFloat "dx1_0"),FVec [(0,151)] (Scalar VI DFloat "dy1_0")]) 
            (ZipT [
              Vec VS (SVec 2 (Scalar VDC DFloat "u_s_1")),
              Vec VS (SVec 3 (Scalar VDC DFloat "w_s_1")),
              Vec VS (SVec 2 (Scalar VDC DFloat "v_s_1"))
              ]) 
          ) 
       ,(Vec VO (Scalar VDC DFloat "w_3"), 
         Map 
            (Function "f_3"  []) 
            (Vec VT (Scalar VDC DFloat "w_1"))
          )
      ]

functionSignaturesList :: [(String,[Expr])]
functionSignaturesList = [
        ("f_1",  [
          Tuple [FVec [(-1,92)] (Scalar VI DFloat "dz1_0"),FVec [(-1,151)] (Scalar VI DFloat "dx1_0"),FVec [(0,151)] (Scalar VI DFloat "dy1_0")],
          Tuple [SVec 5 (Scalar VDC DFloat "u_s_0"),SVec 5 (Scalar VDC DFloat "v_s_0"),SVec 4 (Scalar VDC DFloat "w_s_0")],
          Tuple [Scalar VT DFloat "u_1",Scalar VT DFloat "v_1",Scalar VT DFloat "w_1"]
          ]),
        ("f_2",  [
          Tuple [FVec [(-1,151)] (Scalar VI DFloat "dx1_0"),FVec [(0,151)] (Scalar VI DFloat "dy1_0")],
          Tuple [SVec 2 (Scalar VDC DFloat "u_s_1"),SVec 2 (Scalar VDC DFloat "w_s_1"),SVec 2 (Scalar VDC DFloat "v_s_1")],
          Scalar VO DFloat "v_3"
          ]),
        ("f_3",  [
          Tuple [],
          Scalar VT DFloat "w_1",
          Scalar VO DFloat "w_3"
        ]
        )      
    ]
    -- 
stencilDefinitionsList = [("s1" , [305,23409,23410,23560,23561] ), ("s2" , [23561,23562,23713,46817] ), ("s3" , [305,23409,23560,23561,23712] ), ("s4" , [23409,46665] ), ("s5" , [46665,46816,46817] ), ("s6" , [23560,46816] )]

mainArgDeclsList :: ([(String,DType)],[(String,DType)])
mainArgDeclsList = (
  [("v_0" , DVec 2139552 DFloat )
    , ("w_0" , DVec 2139552 DFloat )
    , ("u_0" , DVec 2139552 DFloat )
    , ("dx1_0" , DFVec [(-1,151)] DFloat )
    , ("dy1_0" , DFVec [(0,151)] DFloat  )
    , ("dz1_0" , DFVec [(-1,92)] DFloat )],
  [("v_3" , DVec 2139552 DFloat )
    , ("w_3" , DVec 2139552 DFloat )]   
  )

astInstance :: ASTInstance
astInstance = (
  "Test 5: Final test case",
  ast,
  functionSignaturesList,
  stencilDefinitionsList,
  mainArgDeclsList)


