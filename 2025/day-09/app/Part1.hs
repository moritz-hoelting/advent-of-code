module Main where

import Day09 (part1)

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ part1 input)
