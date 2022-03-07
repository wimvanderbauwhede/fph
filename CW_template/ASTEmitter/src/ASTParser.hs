-- Do not change this file
-- I encourage you to study it as gives examples of a Parsec parser with and without state, as well as the use of Generics.
module ASTParser (
    parseBindings,
    parseBinding,
    parseMainReturnDef,
    parseMainArgDef,
    parseMainTypeDecl,
    parseFunDecl,
    parseDecl,
    parseStencilDef,
    parseExpr,
    parseDType
    ) where

import AST

import qualified Data.Map.Strict as Map (lookup, insert)

import Data.List (foldl', intercalate) 
import Data.Generics (Data, Typeable, mkQ, mkT, everything, everywhere)

import Text.ParserCombinators.Parsec.Prim (getState, setState, runParser, GenParser)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P -- for makeTokenParser but also all the primitives listed at the end of the source
import Text.ParserCombinators.Parsec.Language (emptyDef)

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Use <$>" #-}
{-# ANN module "HLint: ignore Use zipWith" #-}

type StatefulParser a = GenParser Char ASTInstanceState a

parseMainReturnDef :: String -> [String]
parseMainReturnDef = run_parser main_returndef_parser

parseMainArgDef :: String -> [String]
parseMainArgDef = run_parser main_argdef_parser

parseMainTypeDecl :: String -> ([DType],[DType])
parseMainTypeDecl s = let
    (in_tup, out_tup)  = run_parser main_typedecl_parser s
    in_ts = case in_tup of
        DTuple in_ts  -> in_ts
        in_ts' -> [in_ts']
    out_ts = case out_tup of
        DTuple out_ts  -> out_ts
        out_ts' -> [out_ts']
    in
        ( in_ts, out_ts )

parseFunDecl :: String -> (String,[Expr])
parseFunDecl s = let
    f_decl@(f_name,f_type) = run_parser fdecl_parser s
    in
        if length f_type == 2 then (f_name, Tuple [] : f_type) else f_decl

parseDecl :: String -> (String,DType)
parseDecl = run_parser decl_parser

parseStencilDef :: String -> (String,[Integer])
parseStencilDef = run_parser stencildef_parser

parseDType :: String -> DType
parseDType = run_parser dtype_parser

parseExpr :: String -> Expr
parseExpr = run_parser arg_t_parser

parseBindings :: String -> ASTInstanceState -> [(Expr,Expr)]
parseBindings str = run_parser_state bindings_parser (intercalate ";" $ lines str)

parseBinding :: String -> ASTInstanceState -> (Expr,Expr)
parseBinding = run_parser_state binding_parser

run_parser :: Parser a -> String -> a
run_parser p str =  case parse p "" str of
    Left err -> error $ "parse error at " ++ show err
    Right val  -> val

run_parser_state :: GenParser Char s a -> String -> s -> a
run_parser_state p str state =  case runParser p state "" str of
    Left err -> error $ "parse error at " ++ show err
    Right val  -> val

-- ================================================================================================================================
main_typedecl_parser = do
    symbol "main"
    symbol "::"
    inArgTypes <- dtype_parser
    symbol "->"
    outArgTypes <- dtype_parser
    return (inArgTypes,outArgTypes)

decl_parser :: Parser (String,DType)
decl_parser = do
    fname <- identifier
    symbol "::"
    declType <-  dtype_parser
    return (fname,declType)

fdecl_parser :: Parser (String,[Expr])
fdecl_parser = do
    fname <- identifier
    symbol "::"
    argTypes <-  arg_t_parser `sepBy` symbol "->"
    return (fname,argTypes)

arg_t_parser :: Parser Expr
arg_t_parser =
    try scal_t_parser <|>
    vec_t_parser <|>
    fvec_t_parser <|>
    svec_t_parser <|>
    tuple_parser <?> "Parse error in arg_t_parser"

tuple_parser = do
    exprs <- parens $ commaSep1 arg_t_parser
    return $ Tuple exprs

fvec_t_parser = do
    symbol "FVec"
    szs <- brackets (parens (commaSep1 integer))
    dt <- scal_t_parser
    return $ FVec [(fromInteger (head szs) , fromInteger (szs!!1))] dt

svec_t_parser = do
    symbol "SVec"
    sz <- integer
    dt <- scal_t_parser
    return $ SVec (fromInteger sz) dt

vec_t_parser = do
    symbol "Vec"
    sz <- integer
    dt <- scal_t_parser
    return $ Vec VDC dt

float_t_parser = do
    symbol "Float"
    return $ Scalar VDC DFloat ""

int_t_parser = do
    symbol "Int"
    return $ Scalar VDC DInt ""

scal_t_parser = int_t_parser <|> float_t_parser

dtype_parser =
    try dfloat_parser <|>
    dint_parser <|>
    dvec_parser <|>
    dsvec_parser <|>
    dfvec_parser <|>
    dtuple_parser <?>  "Can't parser DType"

dfloat_parser = do
        symbol "Float"
        return DFloat

dint_parser = do
        symbol "Int"
        return DInt

dvec_parser = do
    symbol "Vec"
    sz <- integer
    DVec (fromInteger sz) <$> dtype_parser

dsvec_parser = do
    symbol "SVec"
    sz <- integer
    DSVec (fromInteger sz) <$> dtype_parser

dfvec_parser = do
    symbol "FVec"
    szs <- brackets (parens (commaSep integer))
    DFVec [(fromInteger (head szs) , fromInteger (szs!!1))] <$> dtype_parser

dtuple_parser =  do
    dts <- parens $ commaSep dtype_parser
    return $ DTuple dts

stencildef_parser :: Parser (String,[Integer])
stencildef_parser = do
    sname <- identifier
    symbol "="
    svals <- brackets (commaSep integer)
    return (sname, svals)

main_argdef_parser :: Parser [String]
main_argdef_parser = do
    symbol  "main "
    inArgs <- arg_parser
    symbol "="
    symbol "let"
    return inArgs

main_returndef_parser :: Parser [String]
main_returndef_parser = do
    whiteSpace
    symbol "in"
    arg_parser

arg_parser = parens (commaSep1 identifier) <|> many1 identifier <?> "Parse error in arg_parser"

-- ================================================================================================================================
context_free_binding_parser :: StatefulParser (Expr,Expr)
context_free_binding_parser = try stencil_binding_parser <|> map_binding_parser

stencil_binding_parser = do
    lhs <- identifier
    symbol "="
    rhs <- stencil_expr_parser
    let
        lhs' = Vec VS (SVec 0 (Scalar VDC DDC lhs ))
        rhs' = Stencil (SVec 0 (Scalar VDC DInt (fst rhs))) (Vec VDC (Scalar VDC DDC (snd rhs)))
    return (lhs',rhs')

stencil_expr_parser = do
    symbol "stencil"
    s <- identifier
    v <-  identifier
    return (s,v)

map_binding_parser = do
    lhs <- arg_parser
    symbol "="
    rhs <- rhs_expr_parser
    let
        lhs'
            | length lhs == 1 = Vec VDC (Scalar VDC DDC (head lhs))
            | otherwise = Tuple $ map (Vec VDC . Scalar VDC DDC) lhs
    return (lhs',rhs)

rhs_expr_parser :: StatefulParser Expr
rhs_expr_parser =
    map_parser <|>
    zipt_parser <|>
    unzipt_parser <|>
    vec_parser
    <?> "Parse error in expr_parser"

map_args_parser = parens zipt_parser <|> vec_parser

map_parser = do
    symbol "map"
    f_expr <- function_parser
    map_args_expr <- map_args_parser
    return $ Map f_expr map_args_expr

unzipt_parser = do
    symbol "unzipt"
    map_expr <-parens map_parser
    return $ UnzipT map_expr

zipt_parser = do
    symbol "zipt"
    vt <- parens (commaSep1 identifier)
    return $ ZipT (map mkVecExpr vt)

vec_tup_parser = do
    vt <- parens (commaSep1 identifier)
    return $ Tuple (map mkVecExpr vt)

vec_parser = do
    vn <- identifier
    return $ mkVecExpr vn

mkVecExpr v_n =  Vec VDC (Scalar VDC DDC v_n)

f_no_nonmap_args = do
    fname <- identifier
    return $ Function fname []
--
f_w_nonmap_arg = do
    fname <- identifier
    non_map_arg <- identifier
    return $ Function fname (map mkFVecExpr [non_map_arg])

f_w_nonmap_args = do
    fname <- identifier
    non_map_args <- parens (commaSep1 identifier)
    return $ Function fname (map mkFVecExpr non_map_args)

-- Function parser parses f | (f dx1) | (f (dx1,...))        
function_parser =  f_no_nonmap_args <|> try (parens f_w_nonmap_arg) <|> parens f_w_nonmap_args

mkFVecExpr fv_n = FVec [] (Scalar VDC DDC fv_n) -- note that these could be bare scalars so lookup needs to test that

{-
Parsec has a problem with newlines, it does not treat them as ordinary characters but as whitespace.
So sepBy binding_parser (string "\n") or similar does nor work.
So I replace the '\n' by a ';' and use semiSep
-}

bindings_parser :: GenParser Char ASTInstanceState [(Expr,Expr)]
bindings_parser = semiSep1 binding_parser

binding_parser :: GenParser Char ASTInstanceState (Expr,Expr)
binding_parser = do
    contextFreeExprTup <- context_free_binding_parser
    state <- getState
    let
        (exprTup,state') = addContextInfo contextFreeExprTup state
    setState state'
    return exprTup

get_rhs_expr :: Expr -> Expr
get_rhs_expr map_expr@(Map f_expr args_expr) = map_expr
get_rhs_expr (UnzipT map_expr) = map_expr
get_rhs_expr vec_expr@(Vec _ _) = vec_expr
get_rhs_expr vec_tup_expr@(Tuple vecs) = vec_tup_expr

update_missing_info_rhs :: ASTInstanceState -> Expr -> Expr
update_missing_info_rhs state = everywhere (mkT (
            update_missing_info_rhs' state
            ))

update_missing_info_rhs' :: ASTInstanceState -> Expr -> Expr
update_missing_info_rhs' state expr = case expr of
    FVec btups (Scalar ve dt sname) -> lookup_fvec_info state sname
    SVec sz (Scalar VDC dt sname) -> lookup_svec_info state sname expr
    Vec sz (SVec ssz (Scalar ve dt vname)) -> lookup_vec_svec_info state vname expr
    Vec sz (Scalar ve dt vname) -> lookup_vec_svec_info state vname expr
    Scalar {} -> expr
    _ -> expr

lookup_fvec_info state sname = let 
        fv = variables state ! sname
        (DFVec btups dt,ve) = fv
    in
        FVec btups (Scalar ve dt sname)

lookup_svec_info state sname expr = case Map.lookup sname (variables state) of
    Just sv ->
        let
            (dtt,VS) = sv
            DSVec ssz dt = dtt
        in
            SVec ssz (Scalar VDC dt sname)
    Nothing -> case Map.lookup sname (stencils state) of
        Just ssz -> let SVec _ (Scalar VDC DInt sname) = expr in SVec ssz (Scalar VDC DInt sname)
        Nothing -> expr

lookup_vec_svec_info state vname expr = case Map.lookup vname (variables state) of
    Just (dtt,ve) -> case ve of
        VS -> let DVec _ (DSVec ssz dt) = dtt in Vec VS (SVec ssz (Scalar VDC dt vname))
        _ -> let DVec _ dt = dtt in Vec ve (Scalar VDC dt vname)
    Nothing -> expr

addContextInfo :: (Expr,Expr) -> ASTInstanceState -> ((Expr,Expr),ASTInstanceState)
addContextInfo contextFreeExprTup state = let
        (lhsExpr,rhsExpr) = contextFreeExprTup
        ((lhsExpr',_),state') = case lhsExpr of
            -- If it is a vector name, it can be a stencil vector and will have been marked VS:        
            Vec VS (SVec _ (Scalar _ _ svname )) -> find_stencil_def_info contextFreeExprTup state
            -- If it is not a stencil, then we have
            Vec _ (Scalar _ _ vname) -> find_vecs_info (Tuple [lhsExpr],rhsExpr) state
            -- Else it's a Tuple,
            Tuple vecs -> find_vecs_info contextFreeExprTup state
            _ -> error "lhsExpr is not a vector or tuple"
        rhsExpr' = update_missing_info_rhs state' rhsExpr
    in
        ((lhsExpr',rhsExpr'),state')

{-
The LHS is either a vector name or a tuple of vector names
* If it is a vector name, it can be a stencil vector and will have been marked VS:
    Vec VS (SVec 0 (Scalar VDC DDC lhs ))
    Stencil (SVec 0 (Scalar VDC DInt (fst rhs))) (Vec VI (Scalar VDC DDC (snd rhs)))
So what we need to find is the DType and the size. 
The size is from the stencil name s
The type is the same type as the vector v
We need to update bindings with new VSs
-}
find_stencil_def_info (lhsExpr,rhsExpr) state = let
        Vec VS (SVec ssz (Scalar _ _ svname )) = lhsExpr
        Stencil (SVec _ (Scalar _ DInt s)) (Vec _ (Scalar _ _ vname)) = rhsExpr
        ssz' = stencils state ! s
        (DVec vsz dt,ve) = variables state ! vname --  (DType,VE) so DVec sz dt => Vec ve (Scalar VDC dt vname) 
        v = Vec ve (Scalar VDC dt vname)
        lhsExpr' = Vec VS (SVec ssz' (Scalar VDC dt svname ))
        rhsExpr' = Stencil (SVec ssz' (Scalar VDC DInt s)) v
        dsv = DVec vsz (DSVec ssz' dt)
        variables' = Map.insert svname (dsv,VS) (variables state)
        state' = state{variables = variables'}
    in
        ((lhsExpr',rhsExpr'),state')
{-
* If it is not a stencil, then we have

Vec VDC (Scalar VDC DDC (head lhs))

* Else it's a Tuple, e.g.
Tuple [Vec VT (Scalar VDC DFloat "u_1"),Vec VT (Scalar VDC DFloat "v_1"),Vec VT (Scalar VDC DFloat "w_1")]

and we need to define VE and DType. 

- VE is based on the name: if it is not in the bindings, it is VT
- DType is based on the return value of the function on the RHS. We get that one using `everything` 
matching on Function. 

so, for every vec in vecs:
- if vec is in the bindings, use the ve from the binding, else put the var in the bindings
- but we can only do that if we know the DType
- So get the DType first

Finally, we have to get the additional information for any variable in the RHS expression, with and everywhere
-}

find_vecs_info (Tuple vecs,rhsExpr) state =
    let
        rhs_expr = get_rhs_expr rhsExpr
        retValExprs = case rhs_expr of
            Map _ retValExprs_ -> retValExprs_
            _ -> rhs_expr
        retValExprs' = update_missing_info_rhs state retValExprs
        dts = get_types retValExprs'
        vecs' = map (\(Vec ve (Scalar vdc DDC vn), dt) ->  Vec ve (Scalar vdc dt vn)) (zip vecs dts)
        -- now we look up vn and 
        -- if it is not in state, we need to update state with a Vec VT 
        -- else we take the VE
        -- as this is over a list, the state update is a fold
        (state',vecs'') = foldl' (\(s_,vs_) vec_@(Vec ve_ (Scalar vdc_ dt_ vn_)) -> case Map.lookup vn_ (variables state)  of
            Just (dt,ve') -> let
                    vec' = Vec ve' (Scalar vdc_ dt_ vn_)
                in
                    (s_,vs_++[vec'])
            Nothing -> let
                    dsv = (DVec 0 dt_,VT)
                    variables' = Map.insert vn_ dsv (variables s_)
                    vec' = Vec VT (Scalar vdc_ dt_ vn_)
                in
                    (s_{variables=variables'},vs_++[vec'])
          ) (state,[]) vecs'
    in
        if length vecs'' == 1
            then
                ((head vecs'',rhsExpr),state')
            else
                ((Tuple vecs'',rhsExpr),state')

get_types :: Expr -> [DType]
get_types (Tuple exprs) = map get_type exprs
get_types (ZipT exprs) = map get_type exprs
get_types expr = [get_type expr]

get_type :: Expr -> DType
get_type (Scalar _ dt _ ) = dt
get_type (SVec _ (Scalar _ dt _ )) = dt
get_type (Vec _ (Scalar _ dt _ )) = dt
get_type (Vec _ (SVec _ (Scalar _ dt _ ))) = dt
get_type e = error $ "Can't get type from "++ show e

-- ================================================================================================================================

lexer       = P.makeTokenParser emptyDef

parens          = P.parens lexer
brackets        = P.brackets lexer
braces          = P.braces lexer
commaSep        = P.commaSep lexer
commaSep1        = P.commaSep1 lexer
semiSep1        = P.semiSep1 lexer
whiteSpace      = P.whiteSpace lexer
symbol          = P.symbol lexer
operator        = P.operator lexer
identifier      = P.identifier lexer
integer         = P.integer lexer
stringLiteral   = P.stringLiteral lexer