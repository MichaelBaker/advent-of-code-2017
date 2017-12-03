module Main where

import Flow ((|>))
import qualified Data.List as List

main = do
  input <- getContents

  input
    |> lines
    |> map (map (read :: String -> Int) . words)
    |> map (List.sortBy (\a b -> if a > b then LT else GT))
    |> map divisiblePair
    |> map (\(a, b) -> div a b)
    |> sum
    |> print

divisiblePair (a:rest) =
  case findDivisor a rest of
    Just divisor -> (a, divisor)
    Nothing -> divisiblePair rest

findDivisor _ [] = Nothing
findDivisor numerator (denominator:rest) =
  if rem numerator denominator == 0
    then Just denominator
    else findDivisor numerator rest
