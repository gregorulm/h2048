{-

h2048
A Haskell implementation of 2048.

Gregor Ulm

last update:
2014-06-18

Please consult the file README for further information
on this program.

-}

import Prelude hiding (Left, Right)
import Data.Char (toLower)
import Data.List
import System.IO
import System.Random
import Text.Printf

data Move = Up | Down | Left | Right
type Grid = [[Int]]

start :: IO Grid
start = do grid'  <- addTile $ replicate 4 [0, 0, 0, 0]
           addTile grid'

merge :: [Int] -> [Int]
merge xs = merged ++ padding
    where padding = replicate (length xs - length merged) 0
          merged  = combine $ filter (/= 0) xs
          combine (x:y:xs) | x == y = x * 2 : combine xs
                           | otherwise = x  : combine (y:xs)
          combine x = x

move :: Move -> Grid -> Grid
move Left  = map merge
move Right = map (reverse . merge . reverse)
move Up    = transpose . move Left  . transpose
move Down  = transpose . move Right . transpose

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
isMoveLeft grid = sum allChoices > 0
    where allChoices = map (length . getZeroes . flip move grid) directions
          directions = [Left, Right, Up, Down]

printGrid :: Grid -> IO ()
printGrid grid = do
    putStr "\ESC[2J\ESC[2J\n" -- clears the screen
    mapM_ (putStrLn . showRow) grid

showRow :: [Int] -> String
showRow = concatMap (printf "%5d")

moves :: [(Char, Move)]
moves = keys "wasd" ++ keys "chtn"
    where keys chars = zip chars [Up, Left, Down, Right]

captureMove :: IO Move
captureMove = do
    inp <- getChar
    case lookup (toLower inp) moves of
        Just x  -> return x
        Nothing -> do putStrLn "Use WASD or CHTN as input"
                      captureMove

check2048 :: Grid -> Bool
check2048 grid = [] /= filter (== 2048) (concat grid)
                
addTile :: Grid -> IO Grid
addTile grid = do
    let candidates = getZeroes grid
    pick <- choose candidates
    val  <- choose [2,2,2,2,2,2,2,2,2,4]
    let new_grid = setSquare grid pick val
    return new_grid

choose :: [a] -> IO a
choose xs = do
    i <- randomRIO (0, length xs-1)
    return (xs !! i)

newGrid :: Grid -> IO Grid
newGrid grid = do
    m <- captureMove
    let new_grid = move m grid
    return new_grid

gameLoop :: Grid -> IO ()
gameLoop grid
    | isMoveLeft grid = do
        printGrid grid
        if check2048 grid
        then putStrLn "You won!"
        else do new_grid <- newGrid grid
                if grid /= new_grid
                then do new <- addTile new_grid
                        gameLoop new
                else gameLoop grid
    | otherwise = do
        printGrid grid
        putStrLn "Game over"

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    grid <- start
    gameLoop grid
