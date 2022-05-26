module Main where

type Point = (Float, Float)

data Expression
  = Point
  | Box
  | Intersection Expression Expression
  | Union Expression Expression
  | Inflated Float Expression
  | Translated Point Expression
  | Scaled Float Expression
  -- | SoftIntersect Float Expression Expression
  -- | SoftUnion Float Expression Expression
  -- | Rotated Float Expression
  -- | Warped Float Expression
  deriving Show

data PreT
  = PlusPre Point
  | MulPre Float
  deriving Show

data PosT
  = PlusPos Float
  | MulPos Float
  deriving Show

data Sdf
  = PointSdf
  | BoxSdf Float -- Argument is horizontal radius. Vertical radius is 1
  | TransformedSdf [PreT] Sdf [PosT]
  | UnionSdf Sdf Sdf
  | IntersectionSdf Sdf Sdf
  deriving Show

exprToSdf :: Expression -> Sdf
exprToSdf Point                   = PointSdf
exprToSdf (Box w)                 = BoxSdf w
exprToSdf (Inflated x e)          = TransformedSdf [] (exprToSdf e) [PlusPos (- x)]
exprToSdf (Translated (dx, dy) e) = TransformedSdf [PlusPre (-dx, -dy)] (exprToSdf e) []
exprToSdf (Scaled x e)            = TransformedSdf [MulPre $ 1.0 / x] (exprToSdf e) [MulPos x]
exprToSdf (Union e1 e2)           = UnionSdf (exprToSdf e1) (exprToSdf e2)
exprToSdf (Intersection e1 e2)    = IntersectionSdf (exprToSdf e1) (exprToSdf e2)


circle :: Point -> Float -> Expression
circle (x, y) r = Translated (x, y) (Inflated r Point)

rectangle :: Point -> Float -> Float -> Expression
rectangle (x, y) w h = Translated (x, y) $ Scaled h $ Box (w / h)

main = return ()
