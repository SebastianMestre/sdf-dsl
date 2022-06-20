module Main where

import Shape

import Control.Monad.State

data FormulaOp1 = SqrtF | SinF | CosF
  deriving Show

data FormulaOp2 = MinF | MaxF | AddF | SubF | MulF | DivF | ModF | AtanF
  deriving Show

data Formula
  = LetF String Formula Formula
  | App2F FormulaOp2 Formula Formula
  | App1F FormulaOp1 Formula
  | VarF String
  | ConstF Float
  deriving Show

data TacOp1 = SqrtS | SinS | CosS
  deriving Show

data TacOp2 = MinS | MaxS | AddS | SubS | MulS | DivS | ModS | AtanS
  deriving Show

data Tac
  = App2S TacOp2 Int Int
  | App1S TacOp1 Int
  | VarS String
  | ConstS Float
  deriving Show

-- Shape to SDF formula translation

instance Num Formula where
  x + y    = App2F AddF x y
  x * y    = App2F MulF x y
  x - y    = App2F SubF x y
  negate x = App2F SubF (ConstF 0) x
  abs         = undefined
  fromInteger = undefined
  signum      = undefined

vx = VarF "x"
vy = VarF "y"
vz = VarF "z"

maxF = App2F MaxF
minF = App2F MinF
modF = App2F ModF
atanF = App2F AtanF

sqrtF = App1F SqrtF
cosF = App1F CosF
sinF = App1F SinF

extrude x w = VarF x - clamped
  where clamped = maxF (-w') (minF w' (VarF x))
        w'      = ConstF w

pythagoras x y z =
  LetF "x2" (x * x) $
  LetF "y2" (y * y) $
  LetF "z2" (z * z) $
  sqrtF (VarF "x2" + VarF "y2" + VarF "z2")

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
  LetF "x" (modF (vx + offset) (ConstF interval) - offset) $
  expand s
    where
      offset = VarF "offset"
expand (UnionS s1 s2) =
  minF (expand s1) (expand s2)
expand (RepeatedXyS times s) =
  LetF "offset" (ConstF (interval / 2)) $
  LetF "angle"  (atanF vy vx) $
  LetF "angle'" (modF (VarF "angle" + offset) (ConstF interval) - offset) $
  LetF "radius" (pythagoras vx vy (ConstF 0)) $
  LetF "x"      ((VarF "radius") * (cosF (VarF "angle'"))) $
  LetF "y"      ((VarF "radius") * (sinF (VarF "angle'"))) $
  expand s
    where
      twopi = 6.28318530718
      interval = twopi / fromIntegral times
      offset = VarF "offset"

-- SDF formula to SSA

type S = ([Tac], Int)
type Env = [(String, Int)]

addTac :: Tac -> State S Int
addTac x = do
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
      i1 <- addTac (ConstS x)
      return i1
    go (App1F op f) e = do
      i1 <- go f e
      addTac (App1S (translateOp1 op) i1)
    go (App2F op f1 f2) e = do
      i1 <- go f1 e
      i2 <- go f2 e
      addTac (App2S (translateOp2 op) i1 i2)

    translateOp1 SqrtF = SqrtS
    translateOp1 SinF = SinS
    translateOp1 CosF = CosS

    translateOp2 MinF = MinS
    translateOp2 MaxF = MaxS
    translateOp2 AddF = AddS
    translateOp2 SubF = SubS
    translateOp2 MulF = MulS
    translateOp2 DivF = DivS
    translateOp2 ModF = ModS
    translateOp2 AtanF = AtanS

-- SSA to Javascript

tacToJavascript :: [Tac] -> [String]
tacToJavascript xs = ["const " ++ emitV n ++ " = " ++ emit x ++ ";\n" | (x, n) <- zip xs [0..]]
  where
    emit :: Tac -> String
    emit (App2S op a b) = emitApp2 op a b
    emit (App1S op a)   = emitApp1 op a
    emit (ConstS x)     = show x
    emit (VarS s)       = s

    emitApp2 MinS a b  = emitF2 "Math.min" a b
    emitApp2 MaxS a b  = emitF2 "Math.max" a b
    emitApp2 AtanS a b = emitF2 "Math.atan2" a b
    emitApp2 AddS a b  = emitBinop "+" a b
    emitApp2 SubS a b  = emitBinop "-" a b
    emitApp2 MulS a b  = emitBinop "*" a b
    emitApp2 DivS a b  = emitBinop "/" a b
    emitApp2 ModS a b  = emitF2 "mod" a b

    emitApp1 SqrtS a   = emitF1 "Math.sqrt" a
    emitApp1 SinS a    = emitF1 "Math.sin" a
    emitApp1 CosS a    = emitF1 "Math.cos" a

    emitF1 f a = f ++ "(" ++ emitV a ++ ")"
    emitF2 f a b = f ++ "(" ++ emitV a ++ "," ++ emitV b ++ ")"
    emitBinop op a b = emitV a ++ op ++ emitV b

    emitV :: Int -> String
    emitV n = "v" ++ show n

compile :: Shape -> String
compile e = concat $ tacToJavascript code
  where
    expanded = expand e
    (_, (code, _)) = lower expanded

main = return ()

