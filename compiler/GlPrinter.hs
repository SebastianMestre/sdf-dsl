module GlPrinter
  ( GlExpr
  , GlStmt
  , GlName
  , GlType

  -- constructores

  -- sentencias
  , glSeq
  , glDecl
  , glConstDecl

  -- expresiones
  , glFieldAccess
  , glBinop
  , glCallExpr
  , glFloatLiteral
  , glNameExpr

  -- nombres
  , glName

  -- tipos
  , glFloat
  , glVec3
  , glMat3

  -- destructores

  , showStmt
  , showExpr
  , showType
  , showName

  ) where

import Data.List

-- Este modulo implementa un DSL Shallow para imprimir
-- codigo GLSL.
-- 
-- Sintacticamente, GLSL es un lenguaje estilo C. Para mas
-- detalle, aca se da una gramatica de GLSL simplificada.
--
-- Stmt ::= Stmt Stmt                                [seq]
--        | Type Name "=" Expr ";"                  [decl]
--        | "const" Type Name "=" Expr ";"    [const-decl]
--
-- Expr ::= Expr Op Expr                           [binop]
--        | Name "(" Expr ("," Expr)* ")"           [call]
--        | Expr "." Name                   [field-access]
--
-- Op ::= "+" | "-" | "*" | ...

newtype GlStmt = GlStmt { showStmt :: String }
newtype GlExpr = GlExpr { showExpr :: String }
newtype GlName = GlName { showName :: String }
newtype GlType = GlType { showType :: String }

glSeq :: GlStmt -> GlStmt -> GlStmt
glSeq s1 s2 = GlStmt $ showStmt s1 ++ "\n" ++ showStmt s2

glDecl :: GlType -> GlName -> GlExpr -> GlStmt
glDecl ty name expr = GlStmt $ concat [showType ty, " ", showName name, " = ", showExpr expr, ";"]

glConstDecl :: GlType -> GlName -> GlExpr -> GlStmt
glConstDecl ty name expr = GlStmt $ "const " ++ (showStmt $ glDecl ty name expr)


glFieldAccess :: GlName -> GlExpr -> GlExpr
glFieldAccess field lhs = GlExpr $ (showExpr lhs) ++ "." ++ (showName field)

glBinop :: String -> GlExpr -> GlExpr -> GlExpr
glBinop op lhs rhs = GlExpr $ (showExpr lhs) ++ " " ++ op ++ " " ++ (showExpr rhs)

glCallExpr :: GlName -> [GlExpr] -> GlExpr
glCallExpr func args = GlExpr $ (showName func) ++ "(" ++ (concat $ intersperse ", " $ map showExpr args) ++ ")"

glFloatLiteral :: Float -> GlExpr
glFloatLiteral x = GlExpr $ show x

glNameExpr :: GlName -> GlExpr
glNameExpr x = GlExpr $ showName x


glName :: String -> GlName
glName x = GlName x


glFloat :: GlType
glFloat = GlType "float"

glVec3 :: GlType
glVec3 = GlType "vec3"

glMat3 :: GlType
glMat3 = GlType "mat3"
