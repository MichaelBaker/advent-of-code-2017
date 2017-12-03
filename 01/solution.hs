module Main where

import Flow ((|>))
import Data.Char (digitToInt)

main = do
  input <- getContents

  input
    |> init
    |> map digitToInt
    |> circularInput
    |> pairs
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
