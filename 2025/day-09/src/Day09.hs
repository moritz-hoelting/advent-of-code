module Day09 (part1, part2) where

import           Data.Char (isSpace)


parsePairs :: String -> Either String [(Int, Int)]
parsePairs input = traverse parseLine indexedLines
  where
    ls = filter (not . all isSpace) (lines input)
    indexedLines = zip [1..] ls

    parseLine :: (Int, String) -> Either String (Int, Int)
    parseLine (n, line) =
      case splitOnce ',' (strip line) of
        Nothing ->
          Left $ "Line " ++ show n ++ ": expected one comma, got: " ++ show line
        Just (aStr, bStr) ->
          case (readMaybeInt aStr, readMaybeInt bStr) of
            (Nothing, _) ->
              Left $ "Line " ++ show n ++ ": left value is not an Int: " ++ show aStr
            (_, Nothing) ->
              Left $ "Line " ++ show n ++ ": right value is not an Int: " ++ show bStr
            (Just a, Just b) ->
              Right (a, b)

    splitOnce :: Char -> String -> Maybe (String, String)
    splitOnce c s =
      case break (== c) s of
        (_, [])          -> Nothing
        (before, _:rest) -> Just (strip before, strip rest)

    strip :: String -> String
    strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

    readMaybeInt :: String -> Maybe Int
    readMaybeInt s =
      case reads s of
        [(x, "")] -> Just x
        _         -> Nothing

twoCombs :: [a] -> [(a, a)]
twoCombs []     = []
twoCombs (x:xs) = map (\y -> (x, y)) xs ++ twoCombs xs

calculateArea :: ((Int, Int), (Int, Int)) -> Int
calculateArea ((x1, y1), (x2, y2)) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

part1 :: String -> Int
part1 input = do
    case parsePairs(input) of
        Left _    -> do
            -1
        Right pairs -> do
            maximum $ map calculateArea $ twoCombs pairs


computeEdges :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
computeEdges [] = []
computeEdges (p:ps) =
    let pts = p : ps
        rotated = ps ++ [p]
    in zip pts rotated

sortTwo :: (Ord a) => (a, a) -> (a, a)
sortTwo (a, b) = if a <= b then (a, b) else (b, a)

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

intersects :: [((Int, Int), (Int, Int))] -> (Int, Int, Int, Int) -> Bool
intersects edges (xMin, xMax, yMin, yMax) =
    any edgeIntersects edges
  where
    edgeIntersects ((x1, y1), (x2, y2)) =
        let (exMin, exMax) = sortTwo (x1, x2)
            (eyMin, eyMax) = sortTwo (y1, y2)
        in not (exMax <= xMin || exMin >= xMax || eyMax <= yMin || eyMin >= yMax)

part2comp :: [((Int, Int), (Int, Int))] -> ((Int, Int), (Int, Int)) -> Int -> Int
part2comp edges ((x1, y1), (x2, y2)) res = do
  let (xMin, xMax) = sortTwo (x1, x2)
      (yMin, yMax) = sortTwo (y1, y2)
      dist = manhattanDistance (x1, y1) (x2, y2)
  if dist * dist > res
  then if intersects edges (xMin, xMax, yMin, yMax)
        then res
        else max res (calculateArea ((x1, y1), (x2, y2)))
  else res

part2 :: String -> Int
part2 input = do
    case parsePairs(input) of
        Left _    -> do
            -1
        Right pairs -> do
            let combinations = twoCombs pairs
            let edges = computeEdges pairs
            foldr (part2comp edges) 0 combinations
