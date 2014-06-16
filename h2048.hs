{-

A Haskell implementation of 2048.

Gregor Ulm
2014-06-15

Please consult the file README for further information
on this program.

-}

import Prelude hiding (Left, Right)
import Data.List
import Text.Printf
import Data.Char (toLower)
import System.Random
import System.IO

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
getZeroes grid = [ (i,j) | (row,i) <- grid `zip` [0..], (0,j) <- row `zip` [0..] ]

setSquare :: Grid -> (Int, Int) -> Int -> Grid
setSquare grid coord val = i
    [ [ if (i,j) == coord then val else old
      | (old,j) <- row `zip` [0..]
      ]
    | (row,i) <- grid `zip` [0..]
    ]

isMoveLeft :: Grid -> Bool
isMoveLeft grid = sum possibilities > 0
    where possibilities = map (length . getZeroes . move grid) [Left, Right, Up, Down]

printGrid :: Grid -> IO ()
printGrid grid = do
    putStrLn $ "\ESC[2J"  ++ "\ESC[2J" --- clears the screen
    mapM_ (putStrLn . showRow) grid

showRow :: [Int] -> String
showRow = concatMap (printf "%5d")

moves = move "wasd" ++ move "chtn"
  where move chars = chars `zip` [Up, Left, Down, Right]

captureMove :: IO Move
captureMove = do
    inp <- getChar
    case lookup (toLower inp) moves of
        Just move -> return move
        Nothing   -> do putStrLn "Use WASD or CHTN as input"
                        captureMove

check2048 :: Grid -> Bool
check2048 grid = [] /= filter (== 2048) (concat grid)

addTile :: Grid -> IO Grid
addTile grid = do
    let candidates = getZeroes grid
    pick <- randomElement candidates
    val <- randomElement [2,2,2,2,2,2,2,2,2,4]
    let new_grid = setSquare grid pick val
    return new_grid

randomElement :: [a] -> IO a
randomElement xs = do
    i <- randomRIO (0, length xs-1)
    return (xs !! i)

newGrid :: Grid -> IO Grid
newGrid grid = do m <- captureMove
                  let new_grid = move grid m
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
    gameLoop start
