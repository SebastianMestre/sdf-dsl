module VarLookup (Lookup, empty, get, extend) where

import Data.Maybe

empty :: Lookup a
get :: String -> Lookup a -> a
extend :: (String, a) -> Lookup a -> Lookup a

newtype Lookup a = LookupImpl [(String, a)]

empty      = wrap []
get x l    = unsafeLookup x $ unwrap l
extend x l = wrap (x : unwrap l)

unwrap (LookupImpl xs) = xs
wrap xs = LookupImpl xs

unsafeLookup :: String -> [(String, a)] -> a
unsafeLookup x = fromJust . lookup x
