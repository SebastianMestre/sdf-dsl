module Main where

type Scene = [Shape]

type Point = (Float, Float)

data Expression
  = Point
  | Box
  -- | Intersect Expression Expression
  -- | Union Expression Expression
  | Inflated Float Expression
  | Translated Float Expression
  -- | Scaled Float Expression
  -- | SoftIntersect Float Expression Expression
  -- | SoftUnion Float Expression Expression
  -- | Rotated Float Expression
  -- | Warped Float Expression

data PreT
  = PlusPre Point
  | MulPre Float

data PosT
  = PlusPos Float
  | MulPos Float

data Sdf
  = PointS
  | BoxS
  | TransformedS [PreT] Sdf [PosT]
  | UnionS Sdf Sdf



data Equation
  = Id String
  | Lit Float
  | Add Equation Equation
  | Sub Equation Equation
  | Mul Equation Equation
  | Div Equation Equation
  | Sqrt Equation
  | Log Equation
  | Exp Equation
  | Abs Equation
  | Pow Equation Equation
  | Max Equation Equation
  | Min Equation Equation

-- compiles an equation to a LaTeX math expression
compileEq :: Equation -> String
compileEq (Lit x) = "(" ++ show x ++ ")"
compileEq (Id x) = x
compileEq (Add e1 e2) = compileEq e1 ++ "+" ++ compileEq e2
compileEq (Sub e1 e2) = compileEq e1 ++ "-" ++ compileEq e2
compileEq (Mul e1 e2) = "(" ++ compileEq e1 ++ ") \\cdot (" ++ compileEq e2 ++ ")"
compileEq (Div e1 e2) = "\\frac{" ++ compileEq e1 ++ "}{" ++ compileEq e2 ++ "}"
compileEq (Sqrt e) = "\\sqrt{" ++ compileEq e ++ "}"
compileEq (Log e) = "\\ln(" ++ compileEq e ++ ")"
compileEq (Exp e) = "\\exp(" ++ compileEq e ++ ")"
compileEq (Abs e) = "abs(" ++ compileEq e ++ ")"
compileEq (Pow e1 e2) = "(" ++ compileEq e1 ++ ")^{" ++ compileEq e2 ++ "}"
compileEq (Max e1 e2) = "\\max(" ++ compileEq e1 ++ "," ++ compileEq e2 ++ ")"

square e = Pow e $ Lit 2
distance e1 e2 = Abs $ Sub e1 e2

compile :: Shape -> Equation
compile (Circle (x, y) r) = Sub (Sqrt $ Add (square $ Sub (Id "x") (Lit x)) (square $ Sub (Id "y") (Lit y))) (Lit r)
compile (Box (x, y) hr vr) = Max (Sub (distance (Id "x") (Lit x)) (Lit hr)) (Sub (distance (Id "y") (Lit y)) (Lit vr))
compile (SoftIntersect s1 s2 r) = Div (Log $ Add (Exp $ Mul (Lit r) v1) (Exp $ Mul (Lit r) v2)) (Lit r)
  where
    v1 = compile s1
    v2 = compile s2
compile (SoftUnion s1 s2 r) = compile $ SoftIntersect s1 s2 (-r)

example = Rotated (SoftUnion c b 10) 1.2
  where
    c = Circle ( 1,  1) 1.5
    b = Box    (-1, -1) 1.5 1

main = putStrLn $ compileEq $ compile example
