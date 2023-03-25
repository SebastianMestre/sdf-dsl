{-

Este modulo define una estructura de datos abstracta para
ligar nombres a valores.

-}
module VarLookup
  ( Lookup
  , empty   -- Estructura vacia.
  , get     -- Busca un valor por nombre. Diverge si no existe el nombre.
  , extend  -- Inserta un nuevo par (nombre, valor)
  ) where

import Data.Maybe

newtype Lookup a = LookupImpl [(String, a)]

empty :: Lookup a
empty = wrap []

get :: String -> Lookup a -> a
get x l = unsafeLookup x $ unwrap l

extend :: (String, a) -> Lookup a -> Lookup a
extend x l = wrap (x : unwrap l)

unwrap (LookupImpl xs) = xs
wrap xs = LookupImpl xs

unsafeLookup :: String -> [(String, a)] -> a
unsafeLookup x = fromJust . lookup x
