module Main where

import Flow ((|>))

main = do
  input <- getContents

  input
    |> lines
    |> map (map (read :: String -> Int) . words)
    |> map (\nums -> (minimum nums, maximum nums))
    |> map (\(min, max) -> max - min)
    |> sum
    |> print
