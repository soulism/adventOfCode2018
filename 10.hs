import Data.Char
import Data.Ord
import Data.List

main = do
    input <- readFile "10.ex"
    print.part2 $ input

data Point = Point {
    position :: (Int, Int),
    velocity :: (Int, Int)
} deriving (Show, Eq)

pointFrom x y z w = Point{position = (x, y), velocity = (z, w)}

part2 = take 5 . iterate (map (positionAfter 1)) . map parse . lines

-- position=< 9,  1> velocity=< 0,  2>
parse :: String -> (Int, Int, Int, Int)
parse line = (x, y, xv, yv)
    where
        x = read $ trim $ takeWhile (/= ',') $ drop 1 $ dropWhile (/= '<') line :: Int
        y = read $ trim $ takeWhile (/= '>') $ drop 1 $ dropWhile (/= ',') line :: Int
        xv = read $ trim $ takeWhile (/= ',') $ drop 1 $ dropWhile (/= '<') $ dropWhile (/= '>') line :: Int
        yv = read $ trim $ takeWhile (/= '>') $ drop 1 $ dropWhile (/= ',') $ dropWhile (/= '>') line :: Int

trim :: String -> String
trim = takeWhile (not.isSpace) . dropWhile (isSpace)

positionAfter :: Int -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
positionAfter t (x, y, xv, yv) = (x + t * xv, y + t * yv, xv, yv)

xCoor :: (Int, Int, Int, Int) -> Int
xCoor (x, _, _, _) = x

yCoor :: (Int, Int, Int, Int) -> Int
yCoor (_, y, _, _) = y

containsMessage :: [(Int, Int, Int, Int)] -> Bool
containsMessage xs = length xAligned > 0 || length yAligned > 0
    where
        xAligned = filter (> 5) . map length . groupBy (\p1 p2 -> xCoor p1 == xCoor p2) . sortBy (comparing xCoor) $ xs
        yAligned = filter (> 5) . map length . groupBy (\p1 p2 -> yCoor p1 == yCoor p2) . sortBy (comparing yCoor) $ xs