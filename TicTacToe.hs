--unbeatable tic tac toe

-- Import modules

import Data.Char
import Data.List
import System.IO

--size of the grid 3 x 3

size :: Int
size = 3


type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

-- Grid utilities

empty :: Grid
empty = replicate size (replicate size B)-- generates [B,B,B]-> generates-> [[B,B,B],[B,B,B],[B,B,B]]


iffull :: Grid -> Bool
iffull = all (/= B) . concat--returns True if none are B in the concatenated list of 9 elements

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
            os = length (filter (== O) ps)--number of O's
            xs = length (filter (== X) ps)--number of X's
            ps = concat g--list of 9 players


diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]-- returns diagonal elements of the grid as a list of 3 players


wins :: Player -> Grid -> Bool -- a function to check if p appears in a triad in any row, column, diagonals.
wins p g = any line (rows ++ cols ++ diags)
           where
              line = all (== p)
              rows = g
              cols = transpose g
              diags = [diag g, diag (map reverse g)]-- a list of major diagonal elements and minor diagonal elements

--map reverse g changes col 1 to col 3 and col 3 to col 1 .


ifwon :: Grid -> Bool--if either O or X has a triad, it returns True
ifwon g = wins O g || wins X g

-- Displaying a grid

--I/O interface

putGrid :: Grid -> IO ()
putGrid =
   putStrLn . unlines . concat . ins bar . map showRow
   where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . ins bar . map showPlayer
          where
             beside = foldr1 (zipWith (++))
             bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

ins :: a -> [a] -> [a]
ins x []     = []
ins x [y]    = [y]
ins x (y:ys) = y : x : ins x ys -- takes an element and a list and inserts the element as the second element of the given list


-- Making a move

valid :: Grid -> Int -> Bool -- to check if the move is valid (boxes doesnt get overwritten)
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move:: Grid -> Int -> Player -> [Grid]-- this function returns a list of grids-> if the list is a singelton -> success else empty list->failure-- helps in making the game tree
move g i p =
   if valid g i then [rettogrid size (xs ++ [p] ++ ys)] else []
   where (xs,B:ys) = splitAt i (concat g)

rettogrid :: Int -> [a] -> [[a]]--returns a list of list to return to original grid format
rettogrid n [] = []
rettogrid n xs = take n xs : rettogrid n (drop n xs)

-- Reading a natural number

getNat :: String -> IO Int-- checking if digit
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then--check if its a digit and non-empty
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt


prompt :: Player -> String-- prompt for input number
prompt p = "Player " ++ show p ++ ", enter your move: "

cls :: IO ()-- clear screen
cls = putStr "\ESC[2J"

goto :: (Int,Int) -> IO ()-- location in the screen ( to start printing hyphens and bars)
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Game trees

data Tree a = Node a [Tree a]-- a here will be a grid
              deriving Show

gametree :: Grid -> Player -> Tree Grid-- generate game tree
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]-- returns a list of grids
moves g p | ifwon g     = []--match ends
          | iffull g    = []--match ends
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a-- think of it like a take function (ftake)
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]


depth :: Int-- default for 3 x 3, but for larger grids it will be useful
depth = 9

-- Minimax

minimax :: Tree Grid -> Tree (Grid,Player)-- uses DFS
minimax (Node g [])
   | wins O g  = Node (g,O) []
   | wins X g  = Node (g,X) []
   | otherwise = Node (g,B) []
minimax (Node g ts)
   | turn g == O = Node (g, minimum ps) ts'
   | turn g == X = Node (g, maximum ps) ts'
                   where
                      ts' = map minimax ts
                      ps  = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Player -> Grid-- calls minimax
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]--
               where
                  tree = prune depth (gametree g p)
                  Node (_,best) ts = minimax tree

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play1 g p

next :: Player -> Player
next O = X
next B = B-- empty box
next X = O

play1 :: Grid -> Player -> IO ()
play1 g p
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | iffull g   = putStrLn "It's a draw!\n"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of
                      []   -> do putStrLn "ERROR: Invalid move"
                                 play1 g p
                      [g'] -> play g' (next p)
   | p == X   = do putStr "Player X is thinking... "
                   (play $! (bestmove g p)) (next p)

