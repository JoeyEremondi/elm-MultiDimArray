module Array.MultiDim
  ( MultiDim
  ) where

{-|

@docs MultiDim
-}

import Array exposing (Array)
import List.Safe

{-| Opaque type for `n`-dimensional arrays containing type `a` -}
type MultiDim a n =
  MD {dims : List.Safe.Safe Int n, arr : Array a}

{- Given the dimensions of an array,
and some coordinates in that array,
find the corresponding position in the "flat"
internal 1D array.
-}
flattenCoords : List.Safe.Safe Int n -> List.Safe.Safe Int n -> Int
flattenCoords safeDims safeCoords =
  let
    dims = List.Safe.toList safeDims
    coords = List.Safe.toList safeCoords
    dimensionOffsets = List.scanl (\x y -> x * y) 1 dims
    addOffset (offset, coord) resultSoFar =
      (offset * coord) + resultSoFar
  in
    List.foldr addOffset 0 <| List.map2 (,) dimensionOffsets coords


{-| Given a SafeList [n1, n2, ...] of array dimensions,
create an n1 x n2 x ... multi-dimensional array -}
repeat : List.Safe.Safe Int n -> a -> MultiDim a n
repeat dims elem =
  let
    arrSize = dims |> List.Safe.toList |> List.product
  in
    MD {dims = dims, arr = Array.repeat arrSize elem}
