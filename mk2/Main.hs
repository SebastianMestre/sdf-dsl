module Main where

type Float3 = (Float, Float, Float)

data Shape
  = PointS
  | TranslatedS Float3 Shape
  | ExtrudedS Float3 Shape
  | InflatedS Float Shape
  | UnionS Shape Shape

sphere radius  = InflatedS radius PointS
box dimensions = ExtrudedS dimensions PointS

data Formula
  = LetF String Formula Formula
  | MinF Formula Formula
  | MaxF Formula Formula
  | AddF Formula Formula
  | SubF Formula Formula
  | MulF Formula Formula
  | DivF Formula Formula
  | SqrtF Formula
  | VarF String
  | ConstF Float

data Ssa
  = MaxS Int Int
  | MinS Int Int
  | AddS Int Int
  | SubS Int Int
  | MulS Int Int
  | DivS Int Int
  | SqrtS Int
  | VarS String
  | ConstS Float

-- Shape to SDF formula translation

extrude x w =  SubF (VarF x) clamped
  where clamped = MaxF minusW (MinF w' (VarF x))
        minusW  = SubF (ConstF 0.0) w'
        w'      = ConstF w

expand :: Shape -> Formula
expand PointS =
  LetF "x2" (MulF (VarF "x") (VarF "x")) $
  LetF "y2" (MulF (VarF "y") (VarF "y")) $
  LetF "z2" (MulF (VarF "z") (VarF "z")) $
  SqrtF (AddF (VarF "x2") $ AddF (VarF "y2") (VarF "z2"))
expand (TranslatedS (dx, dy, dz) s) =
  LetF "x" (SubF (VarF "x") (ConstF dx)) $
  LetF "y" (SubF (VarF "y") (ConstF dy)) $
  LetF "z" (SubF (VarF "z") (ConstF dz)) $
  expand s
expand (InflatedS k s) =
  SubF (expand s) (ConstF k)
expand (ExtrudedS (rx, ry, rz) s) =
  LetF "x" (extrude "x" rx)  $
  LetF "y" (extrude "y" ry)  $
  LetF "z" (extrude "z" rz)  $
  expand s
expand (UnionS s1 s2) =
  MinF (expand s1) (expand s2)


optimize :: Formula -> Formula
optimize (LetF v f1 f2) = LetF v (optimize f1) (optimize f2)
optimize (MinF f1 f2) = MinF (optimize f1) (optimize f2)
optimize (MaxF f1 f2) =
  case (optimize f1, optimize f2) of
    (ConstF k1, MinF (ConstF k2) f3) | k1 == k2 -> ConstF k1
    (f1', f2') -> MaxF f1' f2'
optimize (AddF f1 f2) =
  case (optimize f1, optimize f2) of
    (f1', ConstF 0.0) -> f1'
    (ConstF 0.0, f2') -> f2'
    (ConstF k1, ConstF k2) -> ConstF (k1 + k2)
    (f1', f2') -> AddF f1' f2'
optimize (SubF f1 f2) =
  case (optimize f1, optimize f2) of
    (x', ConstF 0.0) -> x'
    (ConstF k1, ConstF k2) -> ConstF (k1 - k2)
    (x', y') -> SubF x' y'
optimize (MulF f1 f2) = MulF (optimize f1) (optimize f2)
optimize (DivF f1 f2) = DivF (optimize f1) (optimize f2)
optimize (SqrtF f1) = SqrtF (optimize f1)
optimize f = f

-- SDF formula to SSA

type S = ([Ssa], Int)
type Env = [(String, Int)]

addSsa :: (Ssa, S) -> (Int, S)
addSsa (x, (xs, n)) = (n, (xs ++ [x], n+1))

getVar :: String -> [(String, a)] -> a
getVar x ((k, v):kvs)
  | x == k    = v
  | otherwise = getVar x kvs

lower :: Formula -> (Int, S)
lower f = go ((f, defaultEnv), defaultState)
  where
    defaultEnv = [("x", 0), ("y", 1), ("z", 2)]
    defaultState = ([VarS "x", VarS "y", VarS "z"], 3)

    go :: ((Formula, Env), S) -> (Int, S)
    go ((LetF v f1 f2, e), s0) = let
      (i1, s1) = go ((f1, e), s0)
      (i2, s2) = go ((f2, (v, i1):e), s1)
      in (i2, s2)
    go ((VarF v, e), s0) = let
      i1 = getVar v e
      in (i1, s0)
    go ((ConstF x, e), s0) = let
      (i1, s1) = addSsa ((ConstS x), s0)
      in (i1, s1)
    go ((SqrtF f, e), s0) = let
      (i1, s1) = go ((f, e), s0)
      (i2, s2) = addSsa ((SqrtS i1), s1)
      in (i2, s2)
    go ((MinF f1 f2, e), s0) = goBinop f1 f2 e MinS s0
    go ((MaxF f1 f2, e), s0) = goBinop f1 f2 e MaxS s0
    go ((AddF f1 f2, e), s0) = goBinop f1 f2 e AddS s0
    go ((SubF f1 f2, e), s0) = goBinop f1 f2 e SubS s0
    go ((MulF f1 f2, e), s0) = goBinop f1 f2 e MulS s0
    go ((DivF f1 f2, e), s0) = goBinop f1 f2 e DivS s0
    goBinop f1 f2 e op s0 = let
      (i1, s1) = go ((f1, e), s0)
      (i2, s2) = go ((f2, e), s1)
      (i3, s3) = addSsa ((op i1 i2), s2)
      in (i3, s3)

-- SSA to Javascript


codegen :: [Ssa] -> [String]
codegen xs = ["const " ++ emitV n ++ " = " ++ emit x ++ ";\n" | (x, n) <- zip xs [0..]] 
  where
    emit :: Ssa -> String
    emit (MinS a b) = "Math.min(" ++ emitV a ++ "," ++ emitV b ++ ")"
    emit (MaxS a b) = "Math.max(" ++ emitV a ++ "," ++ emitV b ++ ")"
    emit (MulS a b) = emitV a ++ "*" ++ emitV b
    emit (AddS a b) = emitV a ++ "+" ++ emitV b
    emit (SubS a b) = emitV a ++ "-" ++ emitV b
    emit (SqrtS a) = "Math.sqrt(" ++ emitV a ++ ")"
    emit (ConstS x) = show x
    emit (VarS s) = s

    emitV :: Int -> String
    emitV n = "v" ++ show n

compile :: Shape -> String
compile e = concat $ codegen ssa
  where
    expanded = expand e
    (_, (ssa, _)) = lower expanded

compileOptimized :: Shape -> String
compileOptimized e = concat $ codegen ssa
  where
    expanded = optimize $ expand e
    (_, (ssa, _)) = lower expanded

main = return ()

testShape = UnionS b $ UnionS c s
  where s = ExtrudedS (0.7, 0.0, 0.0) (sphere 0.1)
        c = sphere 0.4
        b = TranslatedS (0.5, -0.3, 0.0) $ InflatedS 0.05 $ box (0.2, 0.6, 1.0)

crown n = foldr1 UnionS [let x = (2*3.141592) * y / n in TranslatedS (cos x, sin x, 0.0) (sphere 0.1) | y <- [1..n]]
