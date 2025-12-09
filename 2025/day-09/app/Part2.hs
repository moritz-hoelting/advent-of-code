module Main where

import Day09 (part2)

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 2: " ++ (show $ part2 input)
