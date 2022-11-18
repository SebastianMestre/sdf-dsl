module Codegen where

import Ir
import Crosscutting
import Data.List

emitJs :: [Tac] -> String
emitJs cs = concat $ map (++"\n") $ map (uncurry render) $ zip [0..] cs
  where

  render idx (ConstT x)                 = renderDecl t idx (show x) where t = ScalarF
  render idx (VarT t x)                 = renderDecl t idx x
  render idx (AppT t f as)              = renderDecl t idx (renderApp t f as)
  render idx (PrjT field a)             = renderDecl t idx (renderPrj field a) where t = ScalarF

  renderDecl ty idx expr                = jsCommented (jsConstDecl (renderAtom (TaVar idx)) expr) (show ty)
  renderAtom (TaVar n)                  = jsIdentifier ("v" ++ show n)
  renderAtom (TaConst x)                = jsNumberLiteral x
  renderApp ScalarF SubF [a0, a1]       = jsBinaryOp "-" (renderAtom a0) (renderAtom a1)
  renderApp ScalarF AddF [a0, a1]       = jsBinaryOp "+" (renderAtom a0) (renderAtom a1)
  renderApp ScalarF ClampF [a0, a1, s2] = jsCallExpr "Math.min" [renderAtom a1, jsCallExpr "Math.max" [renderAtom a0, renderAtom a1]]
  renderApp VectorF MkVecF [a0, a1, a2] = jsArrayLiteral [renderAtom a0, renderAtom a1, renderAtom a2]
  renderApp t f as                      = jsCallExpr (show f) (map renderAtom as)
  renderPrj field a                     = jsIndexAccess (renderField field) (renderAtom a)

  renderField XF = jsNumberLiteral 0
  renderField YF = jsNumberLiteral 1
  renderField ZF = jsNumberLiteral 2

  -- little shallow DSL for javascript syntax
  jsIndexAccess idx lhs = lhs ++ "[" ++ idx ++ "]"
  jsFieldAccess field lhs = lhs ++ "." ++ field
  jsCommented line comment = line ++ "\t\t\t// " ++ comment
  jsConstDecl x e = concat ["const ", x ," = ", e, ";"]
  jsCallExpr f as = f ++ "(" ++ (concat $ intersperse "," as) ++ ")"
  jsBinaryOp op lhs rhs = lhs ++ " " ++ op ++ " " ++ rhs
  jsArrayLiteral as = "[" ++ (concat $ intersperse "," as) ++ "]"
  jsNumberLiteral x = show x
  jsIdentifier x      = x

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

  glFieldAccess field lhs = lhs ++ "." ++ field
  glBinop op lhs rhs = lhs ++ " " ++ op ++ " " ++ rhs
  glDecl ty name expr = concat [ty, " ", name, " = ", expr, ";"]
  glCallExpr func args = func ++ "(" ++ (concat $ intersperse ", " $ args) ++ ")"
  glConstQual ty = "const " ++ ty
  glFloat = "float"
  glVec3 = "vec3"
  glMat3 = "mat3"
  glIdentifier x = x
  glFloatLiteral x = show x
