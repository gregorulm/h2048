{-

A Haskell implementation of 2048.

Gregor Ulm
2014-06-15

Please consult the file README for further information
on this program.
   
-}

import Prelude hiding (Left, Right)
import Data.List
import System.Random

data Move = Up | Down | Left | Right
type Grid = [[Int]]   

start :: Grid
start = [[0, 0, 0, 0],
         [0, 0, 0, 0],
         [0, 0, 0, 2],
         [0, 0, 0, 2]]

merge :: [Int] -> [Int]
merge xs = merged ++ padding
    where padding          = replicate (length xs - length merged) 0
          merged           = combine $ filter (/= 0) xs
          combine (x:y:xs) | x == y    = x * 2 : combine xs
                           | otherwise = x     : combine (y:xs) 
          combine x        = x
                    
move :: Grid -> Move -> Grid
move grid Left  = map merge grid
move grid Right = map (reverse . merge . reverse) grid
move grid Up    = transpose $ move (transpose grid) Left
move grid Down  = transpose $ move (transpose grid) Right

getZeroes :: Grid -> [(Int, Int)]
getZeroes grid = filter (\(row, col) -> (grid!!row)!!col == 0) coordinates
    where singleRow n = zip (replicate 4 n) [0..3]
          coordinates = concatMap singleRow [0..3]

setSquare :: Grid -> (Int, Int) -> Int -> Grid
setSquare grid (row, col) val = pre ++ [mid] ++ post
    where pre  = take row grid
          mid  = take col (grid!!row) ++ [val] ++ drop (col + 1) (grid!!row)
          post = drop (row + 1) grid

isMoveLeft :: Grid -> Bool
isMoveLeft grid = sum possibilities > 0 
    where possibilities = map (length . getZeroes . move grid) [Left, Right, Up, Down] 

printGrid :: Grid -> IO ()
printGrid grid = do putStr $ "\ESC[2J" ++ "\ESC[2J" --- clears the screen
                    mapM_ (putStrLn . printRow) grid

printRow :: [Int] -> String
printRow (x:xs) = front ++ show x ++ printRow xs
    where front = concat $ replicate (5 - length (show x)) " "  
printRow []     = ""

captureMove :: IO Move    
captureMove = do 
    inp <- getLine
    case inp of
        "w" -> return Up
        "a" -> return Left
        "s" -> return Down
        "d" -> return Right
        _   -> do putStrLn "Use WASD (lowercase) as input"
                  captureMove  

check2048 :: Grid -> Bool
check2048 grid = [] /= filter (== 2048) (concat grid)
                
addTile :: Grid -> IO Grid
addTile grid = do g <- newStdGen
                  let candidates      = getZeroes grid
                      pos             = head (randoms g :: [Int]) `mod` length candidates
                      pick            = candidates!!pos
                      val             = [2,2,2,2,2,2,2,2,2,4] !! (head (randoms g :: [Int]) `mod` 10)
                      new_grid        = setSquare grid pick val
                  return new_grid

newGrid :: Grid -> IO Grid
newGrid grid = do m <- captureMove    
                  let new_grid = move grid m                         
                  return new_grid
                   
gameLoop :: Grid -> IO ()
gameLoop grid =
    case isMoveLeft grid of
            True  -> do printGrid grid
                        if check2048 grid
                        then putStrLn "You won!"
                        else do new_grid <- newGrid grid                        
                                if grid /= new_grid
                                then do new <- addTile new_grid
                                        gameLoop new
                                else gameLoop grid
            False -> do printGrid grid
                        putStrLn "Game over"

main :: IO ()
main = gameLoop start
