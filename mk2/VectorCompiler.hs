{-# LANGUAGE FlexibleInstances #-}

module VectorCompiler where

import Shape
import Control.Monad.State
import Data.Maybe
import Data.List
import qualified VarLookup

data Form a
  = LetF a TypeF Name (Form a) (Form a)
  | VarF a Name
  | AppF a FunF [(Form a)]
  | LitF Float
  deriving Show

type Name = String

data FunF = MkVecF | MkMatF | MaxF | MinF | AddF | SubF | MulF | DivF | ClampF | SinF | CosF | AtanF | SqrtF | LengthF | AbsF
  deriving Show

data TypeF = ScalarF | VectorF | MatrixF
  deriving (Eq, Show)


instance Num (Form ()) where
  x + y         = AppF () AddF [x, y]
  x * y         = AppF () MulF [x, y]
  x - y         = AppF () SubF [x, y]
  negate x      = 0 - x
  abs           = undefined
  fromInteger n = LitF (fromInteger n :: Float)
  signum        = undefined


vpos = VarF () "pos"

vecLiteral (x, y, z) = AppF () MkVecF [LitF x, LitF y, LitF z]

matLiteral (vx, vy, vz) = AppF () MkMatF [vecLiteral vx, vecLiteral vy, vecLiteral vz]

makeRotationMatrix :: Float -> (Float3, Float3, Float3)
makeRotationMatrix angle = 
  ((cosAngle, -sinAngle, 0),
   (sinAngle,  cosAngle, 0),
   (0,         0,        1))
  where
  cosAngle = cos (-angle)
  sinAngle = sin (-angle)

negate3 (x, y, z) = (-x, -y, -z)

extrude radii x = v - AppF () ClampF [v, vecLiteral (negate3 radii), vecLiteral radii]
  where v = VarF () x 

expand :: Shape -> Form ()
expand PointS =
  AppF () LengthF [vpos]
expand (TranslatedS delta s) =
  LetF () VectorF "pos" (vpos - vecLiteral delta) $
  expand s
expand (InflatedS k s) =
  expand s - LitF k
expand (ExtrudedS radii s) =
  LetF () VectorF "pos" (extrude radii "pos") $
  expand s
expand (RotatedXyS angle s) =
  LetF () VectorF "pos" (matLiteral (makeRotationMatrix angle) * vpos) $
  expand s
expand (RepeatedXS interval s) = expand s -- TODO
expand (RepeatedXyS times s) = expand s -- TODO
expand (UnionS s1 s2) =
  AppF () MinF [expand s1, expand s2]

-- Add type annotations

type TEnv = [(Name, TypeF)]

-- First is return type, then come all the parameter types
type Overload = [TypeF]

overloadMatches :: Overload -> [TypeF] -> Bool
overloadMatches argTypes overload = argTypes == paramTypes overload

returnType :: Overload -> TypeF
returnType = last

paramTypes :: Overload -> [TypeF]
paramTypes = init

overloadsOf :: FunF -> [Overload]
overloadsOf MkVecF = [[ScalarF, ScalarF, ScalarF, VectorF]]
overloadsOf MkMatF = [[VectorF, VectorF, VectorF, MatrixF]]
overloadsOf MaxF = [[ScalarF, ScalarF, ScalarF], [VectorF, VectorF, VectorF], [MatrixF, MatrixF, MatrixF]]
overloadsOf MinF = [[ScalarF, ScalarF, ScalarF], [VectorF, VectorF, VectorF], [MatrixF, MatrixF, MatrixF]]
overloadsOf AddF = [[ScalarF, ScalarF, ScalarF], [VectorF, VectorF, VectorF], [MatrixF, MatrixF, MatrixF]]
overloadsOf SubF = [[ScalarF, ScalarF, ScalarF], [VectorF, VectorF, VectorF], [MatrixF, MatrixF, MatrixF]]
overloadsOf MulF = [[ScalarF, ScalarF, ScalarF], [VectorF, VectorF, VectorF], [MatrixF, MatrixF, MatrixF]]
overloadsOf DivF = [[ScalarF, ScalarF, ScalarF]]
overloadsOf ClampF = [[ScalarF, ScalarF, ScalarF, ScalarF], [VectorF, VectorF, VectorF, VectorF]]
overloadsOf SinF = [[ScalarF, ScalarF]]
overloadsOf CosF = [[ScalarF, ScalarF]]
overloadsOf AtanF = [[ScalarF, ScalarF, ScalarF]]
overloadsOf SqrtF = [[ScalarF, ScalarF]]
overloadsOf LengthF = [[VectorF, ScalarF]]
overloadsOf AbsF = [[ScalarF, ScalarF], [VectorF, VectorF], [MatrixF, MatrixF]]

type Err = Either String

infer :: TEnv -> Form () -> Err (TypeF, Form TypeF)

infer env (LetF () t x e1 e2) = do
  (t1', e1') <- infer env e1
  if t1' /= t
    then fail "mismatched types"
    else return ()
  (t2', e2') <- infer ((x, t) : env) e2 
  return (t2', LetF t2' t x e1' e2')

infer env (VarF () x) = do
  let mt' = lookup x env
  t' <- if isNothing mt'
    then fail ("undefined variable: " ++ x)
    else return $ fromJust mt'
  return (t', VarF t' x)

infer env (AppF () f as) = do
  as' <- mapM (infer env) as

  let args = map snd as'
  let argTypes = map fst as'

  let overloads = overloadsOf f 
  let viable = filter (overloadMatches argTypes) overloads

  chosen <- case viable of
    [x] -> return x
    []  -> fail ("no overloads for " ++ show f ++ " argument types are: " ++ show argTypes ++ " args are: " ++ show args)
    _   -> fail "ambiguous overloads"

  let t = returnType chosen
  return (t, AppF t f args)
infer env (LitF x) = do
  return (ScalarF, LitF x)

-- Form to three address code (Tac)

type VarId = Int

data TacArg = TaVar VarId | TaConst Float
  deriving Show

data Tac
  = AppT TypeF FunF [TacArg]
  | VarT TypeF Name
  | ConstT Float
  deriving Show

type S = ([Tac], VarId)

type GEnv = VarLookup.Lookup VarId

addTac :: Tac -> State S VarId
addTac x = do
  (xs, n) <- get
  put (xs ++ [x], n+1)
  return n

makeVar :: TacArg -> State S VarId
makeVar (TaVar i) = return i
makeVar (TaConst x) = addTac (ConstT x)

lower :: Form TypeF -> [Tac]
lower f = fst $ snd $ runState (go defaultEnv f) defaultState
  where

  defaultEnv :: GEnv
  defaultEnv = VarLookup.extend ("pos", 0) $ VarLookup.empty

  defaultState :: S
  defaultState = ([VarT VectorF "pos"], 1)

  go :: GEnv -> Form TypeF -> State S TacArg
  go env (LetF t h x f1 f2) = do
    a1 <- go env f1
    i1 <- makeVar a1
    i2 <- go (VarLookup.extend (x, i1) env) f2
    return i2
  go env (VarF t v) = do
    let i1 = VarLookup.get v env
    return $ TaVar i1
  go env (LitF x) = do
    return $ TaConst x
  go env (AppF t op as) = do
    is <- mapM (go env) as
    i <- addTac (AppT t op is)
    return $ TaVar i

emitJs :: [Tac] -> String
emitJs cs = concat $ map (++"\n") $ map (uncurry render) $ zip [0..] cs
  where

  render idx (ConstT x)                 = renderDecl t idx (show x) where t = ScalarF
  render idx (VarT t x)                 = renderDecl t idx x
  render idx (AppT t f as)              = renderDecl t idx (renderApp t f as)

  renderDecl ty idx expr                = jsCommented (jsConstDecl (renderAtom (TaVar idx)) expr) (show ty)
  renderAtom (TaVar n)                  = jsIdentifier ("v" ++ show n)
  renderAtom (TaConst x)                = jsNumberLiteral x
  renderApp ScalarF SubF [a0, a1]       = jsBinaryOp "-" (renderAtom a0) (renderAtom a1)
  renderApp ScalarF AddF [a0, a1]       = jsBinaryOp "+" (renderAtom a0) (renderAtom a1)
  renderApp ScalarF ClampF [a0, a1, s2] = jsCallExpr "Math.min" [renderAtom a1, jsCallExpr "Math.max" [renderAtom a0, renderAtom a1]]
  renderApp VectorF MkVecF [a0, a1, a2] = jsArrayLiteral [renderAtom a0, renderAtom a1, renderAtom a2]
  renderApp t f as                      = jsCallExpr (show f) (map renderAtom as)

  -- little shallow DSL for javascript syntax
  jsCommented line comment = line ++ "\t\t\t// " ++ comment
  jsConstDecl x e = concat ["const ", x ," = ", e, ";"]
  jsCallExpr f as = f ++ "(" ++ (concat $ intersperse "," as) ++ ")"
  jsBinaryOp op lhs rhs = lhs ++ " " ++ op ++ " " ++ rhs
  jsArrayLiteral as = "[" ++ (concat $ intersperse "," as) ++ "]"
  jsNumberLiteral x = show x
  jsIdentifier x      = x

emitGlsl :: [Tac] -> String
emitGlsl cs = concat $ map (++";\n") $ map (uncurry go) $ zip [0..] cs
  where

  go idx (ConstT x)    = showDecl "const float" idx (show x)
  go idx (VarT t x)    = showDecl (showType t)  idx x
  go idx (AppT t f as) = showDecl (showType t)  idx (showFun f ++ "(" ++ argList ++ ")")
    where argList = concat $ intersperse "," $ map showVar as

  showDecl ty idx expr = concat [ty, " ", showVar (TaVar idx), " = ", expr]

  showType ScalarF = "float"
  showType VectorF = "vec3"
  showType MatrixF = "mat3"

  showVar (TaVar n)   = "v" ++ show n
  showVar (TaConst x) = show x


  showFun f = show f
