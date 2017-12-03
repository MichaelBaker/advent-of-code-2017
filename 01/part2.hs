module Main where

import Flow ((|>))
import Data.Char (digitToInt)

main = do
  input <- getContents

  let digits = input |> init |> map digitToInt
  let halfWayDigits = cycle digits |> drop (div (length digits) 2)

  zip digits halfWayDigits
    |> filter (\(a, b) -> a == b)
    |> map fst
    |> sum
    |> print

circularInput [] = []
circularInput [a] = [a]
circularInput list@(a:b:rest) = list ++ [a]

pairs [] = []
pairs [a] = []
pairs (a:b:rest) = (a, b) : pairs (b:rest)
