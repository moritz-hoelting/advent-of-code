module Main (main) where

import System.Exit (exitFailure)
import Day09 (part1, part2)

sample :: String
sample = "7,1\n\
\11,1\n\
\11,7\n\
\9,7\n\
\9,5\n\
\2,5\n\
\2,3\n\
\7,3"

main :: IO ()
main = do
    test1
    test2

test1 :: IO ()
test1 = do
    let res = part1 sample
    if res == 50
        then putStrLn "Part1 passed"
        else do
            putStrLn $ "Part1 failed with " ++ show res
            exitFailure

test2 :: IO ()
test2 = do
    let res = part2 sample
    if res == 24
        then putStrLn "Part2 passed"
        else do
            putStrLn $ "Part2 failed with " ++ show res
            exitFailure
