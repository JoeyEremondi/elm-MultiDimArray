module Array.MultiDim
  ( MultiDim
  ) where

{-|

@docs MultiDim
-}

import Array exposing (Array)
import List.Safe
import Debug

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
    dimensionOffsets =
      List.Safe.scanl (\x y -> x * y) 1 safeDims
      |> List.Safe.tail
    addOffset (offset, coord) resultSoFar =
      (offset * coord) + resultSoFar
    offsetCoordPairs =
      List.Safe.map2 (,) dimensionOffsets safeCoords
  in
    List.foldl addOffset 0 <| List.Safe.toList offsetCoordPairs


{- Given the dimensions of an array,
and an integer,
find the corresponding coordinates that flatten
to that integer
-}

expandCoords : List.Safe.Safe Int n -> Int -> List.Safe.Safe Int n
expandCoords dims flatCoord =
  let
    dimensionOffsets = List.Safe.tail <| List.Safe.scanl (\x y -> x * y) 1 dims
    --Do some modular arithmetic to find the right place
    mapFn (offset, lastRemainder ) =
      (lastRemainder // offset, lastRemainder % offset )
  in
    List.Safe.mapl mapFn flatCoord dimensionOffsets


inBounds : List.Safe.Safe Int n -> List.Safe.Safe Int n -> Bool
inBounds dims coords =
  List.Safe.map2 (,) dims coords
  |> List.Safe.all (\(dim, coord) -> coord >= 0 && coord < dim )

{-| Given a SafeList [n1, n2, ...] of array dimensions,
create an n1 x n2 x ... multi-dimensional array -}
repeat : List.Safe.Safe Int n -> a -> MultiDim a n
repeat dims elem =
  let
    arrSize = dims |> List.Safe.toList |> List.product
  in
    MD
    { dims = dims
    , arr = Array.repeat arrSize elem}


initialize
  :  List.Safe.Safe Int n
  -> (List.Safe.Safe Int n -> a)
  -> MultiDim a n
initialize dims initFn =
  let
    arrSize = dims |> List.Safe.toList |> List.product
    intFn = (expandCoords dims) >> initFn
  in
    MD
    { dims = dims
    , arr = Array.initialize arrSize intFn}


{-| Get the dimensions of an array -}
dims : MultiDim a n -> List.Safe.Safe Int n
dims (MD arr) = arr.dims


{-| Return Just the element if all given coordinates
are within the array's bounds. Otherwise, return nothing. -}
get : List.Safe.Safe Int n -> MultiDim a n -> Maybe a
get coords (MD mdArr) =
  if
    (inBounds mdArr.dims coords)
  then
    case Array.get (flattenCoords mdArr.dims coords) mdArr.arr of
      Nothing -> Debug.crash "In-bounds check doesn't agree with array dims"
      x -> x
  else
    Nothing


{-| Set the element at the given coordinates, if they all
are within the array's bounds. Otherwise, return the original array. -}
set : List.Safe.Safe Int n -> a -> MultiDim a n -> MultiDim a n
set coords elem (MD mdArr as origArr) =
  if
    (inBounds mdArr.dims coords)
  then
    let
      flatCoords = flattenCoords mdArr.dims coords
      newArr = Array.set flatCoords elem mdArr.arr
    in
      MD {mdArr | arr <- newArr}
  else
    origArr
