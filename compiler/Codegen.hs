module Codegen
  ( emitGlsl
  ) where

import Ir
import Crosscutting
import Data.List

emitGlsl :: [Tac] -> String
emitGlsl cs = concat $ map (++"\n") $ map (uncurry go) $ zip [0..] cs
  where

  go idx (ConstT x)    = renderDecl (glConstQual glFloat) idx (show x)
  go idx (VarT t x)    = renderDecl (renderType t) idx x
  go idx (AppT t f as) = renderDecl (renderType t) idx (renderApp f as)
  go idx (PrjT field a) = renderDecl glFloat idx (renderPrj field a)

  renderApp SubF [a0, a1] = glBinop "-" (renderAtom a0) (renderAtom a1)
  renderApp AddF [a0, a1] = glBinop "+" (renderAtom a0) (renderAtom a1)
  renderApp MulF [a0, a1] = glBinop "*" (renderAtom a0) (renderAtom a1)
  renderApp DivF [a0, a1] = glBinop "/" (renderAtom a0) (renderAtom a1)
  renderApp f as = glCallExpr (renderFun f) (map renderAtom as)

  renderPrj field a = glFieldAccess (renderField field) (renderAtom a)

  renderFun LengthF = glIdentifier "length"
  renderFun MkVecF  = glIdentifier "vec3"
  renderFun MkMatF  = glIdentifier "mat3"
  renderFun ClampF  = glIdentifier "clamp"
  renderFun MinF    = glIdentifier "min"
  renderFun MaxF    = glIdentifier "max"
  renderFun ModF    = glIdentifier "mod"
  renderFun MixF    = glIdentifier "mix"
  renderFun AbsF    = glIdentifier "abs"
  -- renderFun f       = show f

  renderDecl ty idx expr = glDecl ty (renderAtom (TaVar idx)) expr

  renderType ScalarF = glFloat
  renderType VectorF = glVec3
  renderType MatrixF = glMat3

  renderAtom (TaVar n)   = glIdentifier ("v" ++ show n)
  renderAtom (TaConst x) = glFloatLiteral x

  renderField XF = glIdentifier "x"
  renderField YF = glIdentifier "y"
  renderField ZF = glIdentifier "z"

-- DSL Shallow para expresiones GLSL
type GLExpr = String
type GLStmt = String
type GLName = String

glFieldAccess :: GLName -> GLExpr -> GLExpr
glFieldAccess field lhs = lhs ++ "." ++ field

glBinop :: String -> GLExpr -> GLExpr -> GLExpr
glBinop op lhs rhs = lhs ++ " " ++ op ++ " " ++ rhs

glDecl :: GLName -> GLName -> GLExpr -> GLStmt
glDecl ty name expr = concat [ty, " ", name, " = ", expr, ";"]

glCallExpr :: GLName -> [GLExpr] -> GLExpr
glCallExpr func args = func ++ "(" ++ (concat $ intersperse ", " $ args) ++ ")"

glConstQual :: GLName -> GLName
glConstQual ty = "const " ++ ty

glFloat :: GLName
glFloat = "float"

glVec3 :: GLName
glVec3 = "vec3"

glMat3 :: GLName
glMat3 = "mat3"

glIdentifier :: String -> GLName
glIdentifier x = x

glFloatLiteral :: Float -> GLExpr
glFloatLiteral x = show x

