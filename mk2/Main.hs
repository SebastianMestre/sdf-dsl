module Main where

import Control.Monad.State

type Float3 = (Float, Float, Float)

-- DSL for expressing shapes
data Shape
  = PointS
  | TranslatedS Float3 Shape
  | ExtrudedS Float3 Shape
  | InflatedS Float Shape
  | UnionS Shape Shape
  | RotatedXyS Float Shape -- rotates in the xy plane
  | RepeatedXS Float Shape -- repeats linearly in the x axis
  | RepeatedXyS Int Shape -- repeats radially in the xy plane

data Formula
  = LetF String Formula Formula
  | MinF Formula Formula
  | MaxF Formula Formula
  | AddF Formula Formula
  | SubF Formula Formula
  | MulF Formula Formula
  | DivF Formula Formula
  | ModF Formula Formula
  | SqrtF Formula
  | SinF Formula
  | CosF Formula
  | AtanF Formula Formula
  | VarF String
  | ConstF Float

data Ssa
  = MaxS Int Int
  | MinS Int Int
  | AddS Int Int
  | SubS Int Int
  | MulS Int Int
  | DivS Int Int
  | ModS Int Int
  | SqrtS Int
  | SinS Int
  | CosS Int
  | AtanS Int Int
  | VarS String
  | ConstS Float

-- Shape to SDF formula translation

instance Num Formula where
  x + y    = AddF x y
  x * y    = MulF x y
  x - y    = SubF x y
  negate x = SubF (ConstF 0) x
  abs         = undefined
  fromInteger = undefined
  signum      = undefined

vx = VarF "x"
vy = VarF "y"
vz = VarF "z"

extrude x w = VarF x - clamped
  where clamped = MaxF (-w') (MinF w' (VarF x))
        w'      = ConstF w

pythagoras x y z =
  LetF "x2" (x * x) $
  LetF "y2" (y * y) $
  LetF "z2" (z * z) $
  SqrtF (VarF "x2" + VarF "y2" + VarF "z2")

expand :: Shape -> Formula
expand PointS = pythagoras vx vy vz
expand (TranslatedS (dx, dy, dz) s) =
  LetF "x" (vx - ConstF dx) $
  LetF "y" (vy - ConstF dy) $
  LetF "z" (vz - ConstF dz) $
  expand s
expand (InflatedS k s) =
  expand s - ConstF k
expand (ExtrudedS (rx, ry, rz) s) =
  LetF "x" (extrude "x" rx)  $
  LetF "y" (extrude "y" ry)  $
  LetF "z" (extrude "z" rz)  $
  expand s
expand (RotatedXyS angle s) =
  LetF "x_" (vx * cosAngle - vy * sinAngle) $
  LetF "y_" (vx * sinAngle + vy * cosAngle) $
  LetF "x" (VarF "x_") $
  LetF "y" (VarF "y_") $
  expand s
    where
      cosAngle = ConstF $ cos (-angle)
      sinAngle = ConstF $ sin (-angle)
expand (RepeatedXS interval s) =
  LetF "offset" (ConstF (interval / 2)) $
  LetF "x" (ModF (vx + offset) (ConstF interval) - offset) $
  expand s
    where
      offset = VarF "offset"
expand (UnionS s1 s2) =
  MinF (expand s1) (expand s2)
expand (RepeatedXyS times s) =
  LetF "offset" (ConstF (interval / 2)) $
  LetF "angle"  (AtanF vy vx) $
  LetF "angle'" (ModF (VarF "angle" + offset) (ConstF interval) - offset) $
  LetF "radius" (pythagoras vx vy (ConstF 0)) $
  LetF "x"      ((VarF "radius") * (CosF (VarF "angle'"))) $
  LetF "y"      ((VarF "radius") * (SinF (VarF "angle'"))) $
  expand s
    where
      twopi = 6.28318530718
      interval = twopi / fromIntegral times
      offset = VarF "offset"



optimize :: Formula -> Formula
optimize (LetF v f1 f2) = LetF v (optimize f1) (optimize f2)
optimize (MinF f1 f2) = MinF (optimize f1) (optimize f2)
optimize (MaxF f1 f2) =
  case (optimize f1, optimize f2) of
    (ConstF k1, MinF (ConstF k2) f3)
      | k1 == k2 -> ConstF k1
    (f1', f2')   -> MaxF f1' f2'
optimize (AddF f1 f2) =
  case (optimize f1, optimize f2) of
    (f1', ConstF 0.0)      -> f1'
    (ConstF 0.0, f2')      -> f2'
    (ConstF k1, ConstF k2) -> ConstF (k1 + k2)
    (f1', f2')             -> AddF f1' f2'
optimize (SubF f1 f2) =
  case (optimize f1, optimize f2) of
    (x', ConstF 0.0)       -> x'
    (ConstF k1, ConstF k2) -> ConstF (k1 - k2)
    (x', y')               -> SubF x' y'
optimize (MulF f1 f2) =
  case (optimize f1, optimize f2) of
    (ConstF 0.0, _)        -> ConstF 0.0
    (_, ConstF 0.0)        -> ConstF 0.0
    (x', ConstF 1.0)       -> x'
    (ConstF 1.0, y')       -> y'
    (ConstF k1, ConstF k2) -> ConstF (k1 * k2)
    (x', y')               -> MulF x' y'
optimize (DivF f1 f2) = DivF (optimize f1) (optimize f2)
optimize (ModF f1 f2) = ModF (optimize f1) (optimize f2)
optimize (AtanF f1 f2) = AtanF (optimize f1) (optimize f2)
optimize (SqrtF f1) = SqrtF (optimize f1)
optimize (SinF f1) = SinF (optimize f1)
optimize (CosF f1) = CosF (optimize f1)
optimize (ConstF x) = ConstF x
optimize (VarF x) = VarF x

-- SDF formula to SSA

type S = ([Ssa], Int)
type Env = [(String, Int)]

addSsa :: Ssa -> State S Int
addSsa x = do
  (xs, n) <- get
  put (xs ++ [x], n+1)
  return n

getVar :: String -> [(String, a)] -> a
getVar x ((k, v):kvs)
  | x == k    = v
  | otherwise = getVar x kvs

lower :: Formula -> (Int, S)
lower f = runState (go f defaultEnv) defaultState
  where
    defaultEnv = [("x", 0), ("y", 1), ("z", 2)]
    defaultState = ([VarS "x", VarS "y", VarS "z"], 3)

    go (LetF v f1 f2) e = do
      i1 <- go f1 e
      i2 <- go f2 ((v, i1) : e)
      return i2
    go (VarF v) e = do
      let i1 = getVar v e
      return i1
    go (ConstF x) e = do
      i1 <- addSsa (ConstS x)
      return i1
    go (SqrtF f) e     = goOp1 f e SqrtS
    go (SinF f) e      = goOp1 f e SinS
    go (CosF f) e      = goOp1 f e CosS
    go (MinF f1 f2) e  = goOp2 f1 f2 e MinS
    go (MaxF f1 f2) e  = goOp2 f1 f2 e MaxS
    go (AddF f1 f2) e  = goOp2 f1 f2 e AddS
    go (SubF f1 f2) e  = goOp2 f1 f2 e SubS
    go (MulF f1 f2) e  = goOp2 f1 f2 e MulS
    go (DivF f1 f2) e  = goOp2 f1 f2 e DivS
    go (ModF f1 f2) e  = goOp2 f1 f2 e ModS
    go (AtanF f1 f2) e = goOp2 f1 f2 e AtanS

    goOp1 f e op = do
      i1 <- go f e
      addSsa (op i1)

    goOp2 f1 f2 e op = do
      i1 <- go f1 e
      i2 <- go f2 e
      addSsa (op i1 i2)

-- SSA to Javascript

codegen :: [Ssa] -> [String]
codegen xs = ["const " ++ emitV n ++ " = " ++ emit x ++ ";\n" | (x, n) <- zip xs [0..]] 
  where
    emit :: Ssa -> String
    emit (MinS a b) = emitF2 "Math.min" a b
    emit (MaxS a b) = emitF2 "Math.max" a b
    emit (AtanS a b) = emitF2 "Math.atan2" a b
    emit (MulS a b) = emitBinop "*" a b
    emit (AddS a b) = emitBinop "+" a b
    emit (SubS a b) = emitBinop "-" a b
    emit (ModS a b) = emitF2 "mod" a b
    emit (SqrtS a) = emitF1 "Math.sqrt" a
    emit (SinS a) = emitF1 "Math.sin" a
    emit (CosS a) = emitF1 "Math.cos" a
    emit (ConstS x) = show x
    emit (VarS s) = s

    emitF1 f a = f ++ "(" ++ emitV a ++ ")"
    emitF2 f a b = f ++ "(" ++ emitV a ++ "," ++ emitV b ++ ")"
    emitBinop op a b = emitV a ++ op ++ emitV b

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

union = foldr1 UnionS

sphere :: Float -> Shape
sphere radius  = InflatedS radius PointS

box :: Float3 -> Shape
box dimensions = ExtrudedS dimensions PointS

roundbox :: Float -> Float3 -> Shape
roundbox radius (dx, dy, dz) = InflatedS radius $ box (dx - radius, dy - radius, dz - radius)

capsule :: Float -> Float -> Shape
capsule radius length = ExtrudedS (length, 0.0, 0.0) (sphere radius)

crown :: Int -> Float -> Float -> Shape
{--
crown n radius = union spheres
  where spheres = [TranslatedS centre (sphere radius) | centre <- centres]
        centres = [(cos angle, sin angle, 0.0) | angle <- angles]
        angles  = [(6.283185 / fromIntegral n) * fromIntegral i | i <- [1..n]]
--}
crown n bigRadius radius = RepeatedXyS n (TranslatedS (bigRadius, 0, 0) (sphere radius))

testShape :: Shape
testShape = union [b, c, s]
  where s = capsule 0.1 0.7
        c = sphere 0.4
        b = TranslatedS (0.5, -0.3, 0.0) $ roundbox 0.05 (0.3, 0.7, 1.1)

pointer :: Shape
pointer = UnionS (roundbox 0.025 (0.1, 0.4, 1.0)) (TranslatedS (0, 0.5, 0) $ sphere 0.1)

rotatedBox :: Shape
rotatedBox = RotatedXyS 0.785398 pointer

smallCrown = crown 12 0.16 0.04

dotRow = RepeatedXS 0.4 smallCrown

dotMatrix = RepeatedXS 0.4 $ RotatedXyS 1.57079632 dotRow
