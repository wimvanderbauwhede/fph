-- In this source file you can add additional test cases.
-- Create them analogous to the examples below and put them in the astInstances list
module ASTInstances ( 
          astInstances
        ) where

import AST

astInstances :: [ASTInstance]
astInstances = [
    astInstance1,astInstance2,astInstance3,astInstance4,astInstance5,
    astInstance6,astInstance7,astInstance8,astInstance9,astInstance10
  ]

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
Test case 3: VO used as input

v_in :: Vec 16 Float
v_out1 :: Vec 16 Float
v_out2 :: Vec 16 Float
f :: Float -> Float
main :: Vec 16 Float -> (Vec 16 Float,Vec 16 Float)
main v_in = let
  v_out1 = v_in
  v_out2 = map f v_out1
in 
  (v_out2,v_out1)

-}
ast3 = [
    (
      Vec VO (Scalar VDC DFloat "v_out1"),     
      Vec VI (Scalar VDC DFloat "v_in")
    ),
    (
      Vec VO (Scalar VDC DFloat "v_out2"),     
      Map (Function "f" []) (Vec VO (Scalar VDC DFloat "v_out1"))
    )
  ]
  
functionSignaturesList3 = [
    ("f",  [
        Tuple [],
        Scalar VO DFloat "v_out1",
        Scalar VO DFloat "v_out1"
      ]
    )
  ]
stencilDefinitionsList3 = []
mainArgDeclsList3 = (
    [("v_in" , DVec 16 DFloat )],
    [("v_out2" , DVec 16 DFloat ),("v_out1" , DVec 16 DFloat )]
  )

astInstance3 = (
  "Test 3: VO used as input",
  ast3,
  functionSignaturesList3,
  stencilDefinitionsList3,
  mainArgDeclsList3)
-- ============================================================================================================================================
{-
Test case 4:  

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
  
ast4 =   [
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

functionSignaturesList4 = [
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
stencilDefinitionsList4 = []
mainArgDeclsList4 = (
    [("u_0" , DVec 16 DFloat ),("v_0" , DVec 16 DFloat )],
    [("u_1" , DVec 16 DFloat ),("v_3" , DVec 16 DFloat )]    
  )    
  
astInstance4 = (
  "Test 4: two functions of tuples",
  ast4,
  functionSignaturesList4,
  stencilDefinitionsList4,
  mainArgDeclsList4)
-- ============================================================================================================================================
{-
Test case 5: VI, VS and VT as input 

v_in :: Vec 16 Float
v_out :: Vec 16 Float
f1 :: Float -> Float
f2 :: (Float,SVec 3 Float,Float) -> Float
main :: Vec 16 Float -> Vec 16 Float
main v_in = let
  v_s = stencil s v_in
  v_t = map f1 v_in
  v_out = map f2 (v_in, v_s, v_t)
in 
  v_out
-}

ast5 = [
    (Vec VS (SVec 3 (Scalar VDC DFloat "v_s" )) , Stencil (SVec 3 (Scalar VDC DInt "s")) (Vec VI (Scalar VDC DFloat "v_in"))),
    (Vec VT (Scalar VDC DFloat "v_t"), 
    Map 
      (Function "f1"  []) 
      (Vec VI (Scalar VDC DFloat "v_in"))
    ),
    (Vec VO (Scalar VDC DFloat "v_out"),
    Map 
      (Function "f2"  []) 
      (ZipT [
        Vec VI (Scalar VDC DFloat "v_in"),
        Vec VS (SVec 3 (Scalar VDC DFloat "v_s")),
        Vec VT (Scalar VDC DFloat "v_t")
        ]) 
    ) 
  ]

functionSignaturesList5 = [
    ("f1",  [
        Tuple [],
        Scalar VI DFloat "v_in",
        Scalar VT DFloat "v_t"
      ]
    ),
    ("f2",  [
        Tuple [],
        Tuple [Scalar VI DFloat "v_in",SVec 3 (Scalar VDC DFloat "v_s"),Scalar VT DFloat "v_t"],
        Scalar VO DFloat "v_out"
      ]
    )    
  ]
stencilDefinitionsList5 = [("s",[-1,0,1])]
mainArgDeclsList5 = (
    [("v_in" , DVec 16 DFloat )],
    [("v_out" , DVec 16 DFloat )]
  )  
  
astInstance5 = (
  "Test 5: VI, VS and VT as input ",
  ast5,
  functionSignaturesList5,
  stencilDefinitionsList5,
  mainArgDeclsList5)  
-- ============================================================================================================================================
{- 
Test case 6:  

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
ast6 =   [
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

functionSignaturesList6 = [
    ("f_1",  [      
      Tuple [],
      Tuple [Scalar VI DFloat "u_0",Scalar VI DFloat "v_0"],
      Scalar VT DFloat "u_1"
      -- Scalar VT DFloat "w_1",
      -- Tuple [Scalar VO DFloat "w_3",Scalar VO DFloat "w_3"]
    ]
    ),
    ("f_2",  [
      Tuple [FVec [(0,7)] (Scalar VDC DFloat "fv_1"),FVec [(0,3)] (Scalar VDC DFloat "fv_2")],
      Tuple [Scalar VI DFloat "v_0",Scalar VT DFloat "u_1"],
      Tuple [Scalar VO DFloat "u_2",Scalar VT DFloat "v_3"]

        -- Tuple [],
        -- Scalar VT DFloat "w_1",
        -- Tuple [Scalar VO DFloat "w_3",Scalar VO DFloat "w_3"]
      ]
    ),
    ("f_3",  [
        Tuple [],
        Tuple [Scalar VT DFloat "v_3",Scalar VI DFloat "u_0"],
        Scalar VO DFloat "v_4"
        -- Scalar VT DFloat "w_1",
        -- Tuple [Scalar VO DFloat "w_3",Scalar VO DFloat "w_3"]
      ]
    ) 
  ]
stencilDefinitionsList6 = []
mainArgDeclsList6 = (
    [("fv_1" , DFVec [(0,7)] DFloat ),
    ("fv_2" , DFVec [(0,3)] DFloat  ),  
    ("u_0" , DVec 16 DFloat ),
    ("v_0" , DVec 16 DFloat )
    ],
    [("u_2" , DVec 16 DFloat ),("v_4" , DVec 16 DFloat )]
  )    
astInstance6 = ( 
  "Test 6: 3 functions of tuples, one with non-map args",
  ast6,
  functionSignaturesList6,
  stencilDefinitionsList6,
  mainArgDeclsList6)
-- ============================================================================================================================================
{-
Test case 7:  Single function, redundant return vec

u_0 :: Vec 16 Float
v_0 :: Vec 16 Float
u_2 :: Vec 16 Float

fv_1 :: FVec [(0,7)] Float
fv_2 :: FVec [(0,3)] Float

f_2 :: (FVec [(0,7)] Float,FVec [(0,3)] Float) -> (Float,Float) -> (Float,Float)

main :: (FVec [(0,7)] Float,FVec [(0,3)] Float,Vec 16 Float,Vec 16 Float) -> Vec 16 Float
main (fv_1,fv_2,u_0,v_0) = let  
  (u_2,v_3) = unzipt (map (f_2 (fv_1,fv_2)) (zipt (v_0,u_0)))
in 
  u_2

-}
ast7 =   [
        (Tuple [Vec VO (Scalar VDC DFloat "u_2"),Vec VT (Scalar VDC DFloat "v_3")],
          UnzipT (
          Map 
            (Function "f_2"  [FVec [(0,7)] (Scalar VI DFloat "fv_1"),FVec [(0,3)] (Scalar VI DFloat "fv_2")]) 
            (ZipT [
              Vec VI (Scalar VDC DFloat "v_0"),
              Vec VI (Scalar VDC DFloat "u_0")
              ]) 
          ) 
        )
  ]

functionSignaturesList7 = [
    ("f_2",  [
        Tuple [FVec [(0,7)] (Scalar VI DFloat "fv_1"),FVec [(0,3)] (Scalar VI DFloat "fv_2")],
        Tuple [Scalar VI DFloat "v_0",Scalar VI DFloat "u_0"],
        Tuple [Scalar VO DFloat "u_2",Scalar VT DFloat "v_3"]
      ]
    )
  ]
stencilDefinitionsList7 = []
mainArgDeclsList7 = (
    [("fv_1" , DFVec [(0,7)] DFloat ),
    ("fv_2" , DFVec [(0,3)] DFloat  ),  
    ("u_0" , DVec 16 DFloat ),
    ("v_0" , DVec 16 DFloat )
    ],
    [("u_2" , DVec 16 DFloat )]
  )    
astInstance7 = ( 
  "Test 7: single function, redundant return vec",
  ast7,
  functionSignaturesList7,
  stencilDefinitionsList7,
  mainArgDeclsList7)  

-- ============================================================================================================================================
{-
Test case 8:  

u_0 :: Vec 16 Float
v_0 :: Vec 16 Float
u_1 :: Vec 16 Float
v_1 :: Vec 16 Float
fv_1 :: FVec [(0,7)] Float
fv_2 :: FVec [(0,3)] Float
f_1 :: Float -> Float
f_2 :: Float -> Float
f_3 :: FVec [(0,7)] Float -> (Float,Float,SVec 3 Float) -> Float
f_4 :: FVec [(0,3)] Float -> (Float,Float,SVec 3 Float) -> Float

main :: (Vec 16 Float,Vec 16 Float) -> Vec 16 Float
main (u_0,v_0) = let
  v1_t = map f_1 u_0
  v2_t = map f_2 v_0
  v_s1 = stencil s1 v2_t
  v_s2 = stencil s2 v1_t
  u_1 = map (f_3 fv_1) (zipt (v1_t,v_0,v_s1))
  v_1 = map (f_4 fv_2) (zipt (v2_t,u_0,v_s2))
in 
  (u_1,v_1)
-}
  
ast8 =   [
    (Vec VT (Scalar VDC DFloat "v1_t"), 
    Map 
      (Function "f_1"  []) 
      (Vec VI (Scalar VDC DFloat "u_0"))
    ),
    (Vec VT (Scalar VDC DFloat "v2_t"), 
    Map 
      (Function "f_2"  []) 
      (Vec VI (Scalar VDC DFloat "v_0"))
    ),        
    (Vec VS (SVec 3 (Scalar VDC DFloat "v_s1" )) , Stencil (SVec 3 (Scalar VDC DInt "s1")) (Vec VT (Scalar VDC DFloat "v2_t"))),
    (Vec VS (SVec 3 (Scalar VDC DFloat "v_s2" )) , Stencil (SVec 3 (Scalar VDC DInt "s2")) (Vec VT (Scalar VDC DFloat "v1_t"))),
    ( Vec VO (Scalar VDC DFloat "u_1"), 
        Map 
        (Function "f_3"  [FVec [(0,7)] (Scalar VDC DFloat "fv_1")]) 
        (ZipT [
            Vec VT (Scalar VDC DFloat "v1_t"),
            Vec VI (Scalar VDC DFloat "v_0"),        
            Vec VS (SVec 3 (Scalar VDC DFloat "v_s1"))
            ]) 
        ),
    (Vec VO (Scalar VDC DFloat "v_1"),
        Map 
        (Function "f_4"  [FVec [(0,3)] (Scalar VDC DFloat "fv_2")]) 
        (ZipT [
            Vec VT (Scalar VDC DFloat "v2_t"),
            Vec VI (Scalar VDC DFloat "u_0"),
            Vec VS (SVec 3 (Scalar VDC DFloat "v_s2"))
            ]) 
        ) 
  ]

functionSignaturesList8 = [
    ("f_1",  [
        Tuple [],
        Scalar VI DFloat "u_0",
        Scalar VT DFloat "v1_t"        
      ]
    ),
    ("f_2",  [
        Tuple [],
        Scalar VO DFloat "v_0",
        Scalar VO DFloat "v2_t"        
      ]
    ), 
    ("f_3",  [
        FVec [(0,7)] (Scalar VDC DFloat "fv_1"),
        Tuple [Scalar VT DFloat "v1_t",Scalar VI DFloat "v_0", SVec 3 (Scalar VS DFloat "v_s1")],
        Scalar VT DFloat "u_1"
        
      ]
    ),
    ("f_4",  [
        FVec [(0,3)] (Scalar VDC DFloat "fv_2"),
        Tuple [Scalar VT DFloat "v2_t",Scalar VI DFloat "u_0", SVec 3 (Scalar VS DFloat "v_s2")],
        Scalar VO DFloat "v_1"        
      ]
    )     
  ]
stencilDefinitionsList8 = [("s1",[-1,0,1]),("s2",[-4,2,4])]
mainArgDeclsList8 = (
    [
        ("u_0" , DVec 16 DFloat ),
        ("v_0" , DVec 16 DFloat ),
        ("fv_1", DFVec [(0,7)] DFloat),
        ("fv_2", DFVec [(0,3)] DFloat)
    ],
    [("u_1" , DVec 16 DFloat ),("v_1" , DVec 16 DFloat )]    
  )    
  
astInstance8 = (
  "Test 8: four functions, two stencils",
  ast8,
  functionSignaturesList8,
  stencilDefinitionsList8,
  mainArgDeclsList8)


-- ============================================================================================================================================
{-
Test case 9: 

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

ast9 :: AST
ast9 = [
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

functionSignaturesList9 :: [(String,[Expr])]
functionSignaturesList9 = [
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
stencilDefinitionsList9 = [("s1" , [305,23409,23410,23560,23561] ), ("s2" , [23561,23562,23713,46817] ), ("s3" , [305,23409,23560,23561,23712] ), ("s4" , [23409,46665] ), ("s5" , [46665,46816,46817] ), ("s6" , [23560,46816] )]

mainArgDeclsList9 :: ([(String,DType)],[(String,DType)])
mainArgDeclsList9 = (
  [("v_0" , DVec 2139552 DFloat )
    , ("w_0" , DVec 2139552 DFloat )
    , ("u_0" , DVec 2139552 DFloat )
    , ("dx1_0" , DFVec [(-1,151)] DFloat )
    , ("dy1_0" , DFVec [(0,151)] DFloat  )
    , ("dz1_0" , DFVec [(-1,92)] DFloat )],
  [("v_3" , DVec 2139552 DFloat )
    , ("w_3" , DVec 2139552 DFloat )]   
  )

astInstance9 :: ASTInstance
astInstance9 = (
  "Test 9: Final test case",
  ast9,
  functionSignaturesList9,
  stencilDefinitionsList9,
  mainArgDeclsList9)
-- ============================================================================================================================================
-- Test case 10: variant of the final test
{-
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
f_t1 :: (Float, SVec 5 Float ) -> Float
f_1 :: (FVec [(-1,151)] Float,FVec [(0,151)] Float,FVec [(-1,92)] Float) -> (SVec 5 Float,SVec 5 Float,SVec 4 Float) -> (Float,Float,Float)
f_2 :: (FVec [(-1,151)] Float,FVec [(0,151)] Float) -> (SVec 2 Float,SVec 3 Float,SVec 2 Float) -> Float
f_t2 :: (Float, SVec 2 Float ) -> Float
f_3 :: Float -> Float

main :: (Vec 2139552 Float,Vec 2139552 Float,Vec 2139552 Float,FVec [(-1,151)] Float,FVec [(0,151)] Float,FVec [(-1,92)] Float) -> (Vec 2074891 Float,Vec 2074891 Float)
main (v_0,w_0,u_0,dx1_0,dy1_0,dz1_0) = let
    -- f_1
    v_s_0 = stencil s1 v_0
    w_t_1 = map f_t1 (zipt (w_0,v_s_0))
    (u_1,v_1,w_1) = unzipt (map (f_1 (dx1_0,dy1_0,dz1_0)) (zipt (u_0,v_s_0,w_t_1)))
    -- f_2
    v_s_1 = stencil s4 v_1
    w_t_2 = map f_t2 (zipt (w_1,v_s_1))
    u_s_1 = stencil s6 u_1
    v_3 = map (f_2 (dx1_0,dy1_0)) (zipt (u_s_1,w_s_1,v_s_1))
    -- f_3
    w_3 = map f_3 w_t_2
  in
    (v_3,w_3)
-}
ast10 :: AST
ast10 = [
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

functionSignaturesList10 :: [(String,[Expr])]
functionSignaturesList10 = [
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
          -- w_3 = map f_3 w_t_2
        ("f_3",  [
          Tuple [],
          Scalar VT DFloat "w_t_2",
          Scalar VO DFloat "w_3"
        ]
        ),
        -- w_t_1 = map f_t1 (zipt (w_0,v_s_0))
        ("f_t1",  [
          Tuple [],
          Tuple [Scalar VI DFloat "w_0",SVec 5 (Scalar VS DFloat "v_s_0")],
          Scalar VT DFloat "w_t_1"
        ]
        ),
        -- w_t_2 = map f_t2 (zipt (w_1,v_s_1))
        ("f_t2",  [
          Tuple [],
          Tuple [Scalar VT DFloat "w_1",SVec 2 (Scalar VS DFloat "v_s_1")],
          Scalar VT DFloat "w_t_2"
        ]
        )                          
    ]
    -- 
stencilDefinitionsList10 = [("s1" , [305,23409,23410,23560,23561] ), ("s2" , [23561,23562,23713,46817] ), ("s3" , [305,23409,23560,23561,23712] ), ("s4" , [23409,46665] ), ("s5" , [46665,46816,46817] ), ("s6" , [23560,46816] )]

mainArgDeclsList10 :: ([(String,DType)],[(String,DType)])
mainArgDeclsList10 = (
  [("v_0" , DVec 2139552 DFloat )
    , ("w_0" , DVec 2139552 DFloat )
    , ("u_0" , DVec 2139552 DFloat )
    , ("dx1_0" , DFVec [(-1,151)] DFloat )
    , ("dy1_0" , DFVec [(0,151)] DFloat  )
    , ("dz1_0" , DFVec [(-1,92)] DFloat )],
  [("v_3" , DVec 2139552 DFloat )
    , ("w_3" , DVec 2139552 DFloat )]   
  )

astInstance10 :: ASTInstance
astInstance10 = (
  "Test case 10: variant of test 9",
  ast10,
  functionSignaturesList10,
  stencilDefinitionsList10,
  mainArgDeclsList10)

-- ============================================================================================================================================
-- END test cases