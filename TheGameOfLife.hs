---------- The Game of Life ----------
import Data.List    
main :: IO()
main = putStrLn (pretty (take 8 (visualisation 5 5 (evolution glider))))


pretty :: [[[Char]]] -> String
pretty []
    = ""
pretty (xs:xss)
    = unlines xs ++ pretty xss


type Point
    = (Int, Int)

glider :: [Point]
glider
    = [ (0, 2), (1, 3), (2, 1), (2, 2), (2, 3) ]
    
addPoint :: [[Char]] -> Point -> [[Char]]
addPoint grid (x,y)
    = take (y) grid ++ [take x (grid !! y) ++ ['#'] ++ 
      drop (x + 1) (grid !! y)] ++ drop (y+1) grid

    
visualisation :: Int -> Int -> [[Point]] -> [[[Char]]]
visualisation w h []
    = []
visualisation w h (xs:xss)
    = (foldl addPoint grid xs) : visualisation w h xss
      where grid = replicate h (replicate w '.')



isAlive :: [[Char]] -> Point -> Int
isAlive grid (x, y)
    | (x < 0) || (x > 4) || (y < 0) || (y > 4) = 0
    | (grid !! y) !! x == '#' = 1
    | otherwise = 0
      
isAliveNext :: [[Char]] -> Point -> Bool
isAliveNext grid (x, y)
    | (((grid !! y) !! x == '#') && ((score == 2) || (score == 3))) ||
      (((grid !! y) !! x == '.') && (score == 3)) = True
    | otherwise = False
      where score = (isAlive grid (x, y - 1) +
                     isAlive grid (x, y + 1) +
                     isAlive grid (x - 1, y) +
                     isAlive grid (x + 1, y) +
                     isAlive grid (x + 1, y + 1) +
                     isAlive grid (x + 1, y - 1) +
                     isAlive grid (x - 1, y - 1) +
                     isAlive grid (x - 1, y + 1))

  
filterPoints :: [[Char]] -> [Point] -> [Point] -> [Point]
filterPoints grid newPoints []
    = newPoints
filterPoints grid newPoints (x:xs)
    | (isAliveNext grid x) = x : filterPoints grid newPoints xs
    | otherwise = filterPoints grid newPoints xs
    

iterate :: [Point] -> [Point]
iterate ps
    = filterPoints grid [] allPoints 
      where grid = (visualisation 5 5 [ps]) !! 0
            allPoints = [(x, y) | x <- [0..4], y <- [0..4]]


evolution :: [Point] -> [[Point]]
evolution ps
    = ps : evolution (Main.iterate ps)