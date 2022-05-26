module Main where

type Float3 = (Float, Float, Float)

data Shape
  = OriginS
  | TranslatedS Float3 Shape
  | ExtrudedS Float3 Shape
  | InflatedS Float Shape
  | UnionS Shape Shape

sphere radius  = InflatedS radius OriginS
box dimensions = ExtrudedS dimensions OriginS

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

type S = ([Ssa], Int)

addSsa :: (Ssa, S) -> (Int, S)
addSsa (x, (xs, n)) = (n, (xs ++ [x], n+1))

translate :: ((Int, Float), S) -> (Int, S)
translate ((x, d), s0) = let
  (d', s1) = addSsa ((ConstS d), s0)
  (x', s2) = addSsa ((SubS x d'), s1)
  in (x', s2)

extrude :: ((Int, Float), S) -> (Int, S)
extrude ((x, w), s0) = let
  (zero, s1) = addSsa ((ConstS 0.0), s0)
  (w',   s2) = addSsa ((ConstS w), s1)
  (mw,   s3) = addSsa ((SubS zero w'), s2)
  (mx,   s4) = addSsa ((SubS zero x), s3)
  (cl1,  s5) = addSsa ((MinS w' mx), s4)
  (cl2,  s6) = addSsa ((MaxS mw cl1), s5)
  (x',   s7) = addSsa ((AddS x cl2), s6)
  in (x', s7)

lower :: ((Shape, Int, Int, Int), S) -> (Int, S)
lower ((OriginS, x, y, z), s0) = let
  (x2, s1) = addSsa (MulS x x, s0)
  (y2, s2) = addSsa (MulS y y, s1)
  (z2, s3) = addSsa (MulS z z, s2)
  (a1, s4) = addSsa (AddS x2 y2, s3)
  (d2, s5) = addSsa (AddS a1 z2, s4)
  (d,  s6) = addSsa (SqrtS d2, s5)
  in (d, s6)
lower ((TranslatedS (dx, dy, dz) s, x, y, z), s0) = let
  (x', s1) = translate ((x, dx), s0)
  (y', s2) = translate ((y, dy), s1)
  (z', s3) = translate ((z, dz), s2)
  (d,  s4) = lower ((s, x', y', z'), s3)
  in (d, s4)
lower ((InflatedS k s, x, y, z), s0) = let
  (d,  s1) = lower ((s, x, y, z), s0)
  (c,  s2) = addSsa ((ConstS k), s1)
  (d', s3) = addSsa ((SubS d c), s2)
  in (d', s3)
lower ((ExtrudedS (rx, ry, rz) s, x, y, z), s0) = let
  (x', s1) = extrude ((x, rx), s0)
  (y', s2) = extrude ((y, ry), s1)
  (z', s3) = extrude ((z, rz), s2)
  (d,  s4) = lower ((s, x', y', z'), s3)
  in (d, s4)
lower ((UnionS f1 f2, x, y, z), s0) = let
  (d1, s1) = lower ((f1, x, y, z), s0)
  (d2, s2) = lower ((f2, x, y, z), s1)
  (d,  s3) = addSsa ((MinS d1 d2), s2)
  in (d, s3)

emitV :: Int -> String
emitV n = "v" ++ show n

emit :: Ssa -> String
emit (MinS a b) = "Math.min(" ++ emitV a ++ "," ++ emitV b ++ ")"
emit (MaxS a b) = "Math.max(" ++ emitV a ++ "," ++ emitV b ++ ")"
emit (MulS a b) = emitV a ++ "*" ++ emitV b
emit (AddS a b) = emitV a ++ "+" ++ emitV b
emit (SubS a b) = emitV a ++ "-" ++ emitV b
emit (SqrtS a) = "Math.sqrt(" ++ emitV a ++ ")"
emit (ConstS x) = show x
emit (VarS s) = s

codegen :: [Ssa] -> [String]
codegen xs = ["const " ++ emitV n ++ " = " ++ emit x ++ ";\n" | (x, n) <- zip xs [0..]] 

compile :: Shape -> String
compile e = concat $ codegen ssa
  where (_, (ssa, _)) = lower ((e, 0, 1, 2), initialState)
        initialState = ([VarS "x", VarS "y", VarS "z"], 3)

main = return ()

testShape = UnionS b $ UnionS c s
  where s = ExtrudedS (0.7, 0.0, 0.0) (sphere 0.1)
        c = sphere 0.4
        b = TranslatedS (0.5, -0.3, 0.0) $ InflatedS 0.05 $ box (0.2, 0.6, 1.0)
