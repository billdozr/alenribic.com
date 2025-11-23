---
title: "Haskell: an attempt at tic-tac-toe"
author: Alen Ribic
date: July 8, 2012
tags: haskell, games
description: Here is a quick tic-tac-toe game I wrote in Haskell this weekend and I'm pretty pleased with my first attempt.
---
Here is a quick tic-tac-toe game I wrote in Haskell this weekend and I'm pretty pleased with my first attempt.

Two key things stuck out for me during the development process: 

 * The ability to **interactively test the functional expressions** in GHCi and,
 * the **clear separation between** the **pure functions** and the **IO monadic actions**.

I started off firstly defining the data structures to represent the game elements such as the board, move and position.

~~~~~{.haskell}
data Move        = O | X 
                 deriving (Eq, Show, Enum, Ord)
type Position    = (Char, Int)
data BoardMove   = BoardMove 
                   { bMove :: Maybe Move, bPos :: Position } 
                 deriving (Eq, Show)
type Board       = [BoardMove]
type InvalidMove = String
~~~~~

Next, the `main` function will initiate the gameloop with a new empty board.

~~~~~{.haskell}
main = do
  putStrLn "Starting new game..."
  putStrLn "Type 'quit' to exit game"
  let newBoard = empty bsize
      in do (putStrLn . (\s->"\n"++s++"\n") . printBoard) newBoard
            gameloop Nothing newBoard
~~~~~

The `empty` function initializes the new game board based on the input size.

~~~~~{.haskell}
bsize = 3
coord = (['A'..], [1..])

empty :: Int -> Board
empty size = do
    x <- take size (fst coord)
    y <- take size (snd coord)
    return $ BoardMove Nothing (x,y)
~~~~~

This is the part I really love about the List implementation of the Monad class.
Note how the Board (BoardMove list synonym) is produced monadically by taking the tuple pairs from the coordinates and producing a Cartesian product.

The beautiful thing is how **the lazy evaluation plays a role** [1] **here in giving us just the number of coordinates we need** based on the board size.

Next, we have a few functions for: making a move on the board, checking if there is a winner or if the game is a draw.

The `move` function is responsible for playing the next move on the board, resulting in either an invalid move or a board with the newly added move.

~~~~~{.haskell}
move :: BoardMove -> Board -> Either InvalidMove Board
move (BoardMove _ (c,r)) [] = 
  Left $ "Could not make the move to given position " ++ [c] ++ (show r)
move bm@(BoardMove nmov npos) (x:xs) 
  | findMove x = Right $ bm:xs
  | otherwise  = 
    case move bm xs of
      Right r -> Right $ x:r
      err     -> err
  where findMove (BoardMove m p) = 
          p == npos && isNothing m && nmov /= Nothing
~~~~~

The `win` and `draw` functions are simply predicate functions for determining if there is a winner or if the game resulted in a draw.

~~~~~{.haskell}
win :: BoardMove -> Board -> Bool
win (BoardMove Nothing _) _ = False
win (BoardMove m (c,r)) b = row || col || diag' cb || diag' (reverse cb)
  where row = length 
              (filter (\(BoardMove m2 (_,r2)) -> 
                        m2 == m && r2 == r) b) == bsize
        col = length
              (filter (\(BoardMove m2 (c2,_)) -> 
                        m2 == m && c2 == c) b) == bsize
        diag' xss = all (\(BoardMove m2 _) -> 
                          m2 == m) $ diag xss
        cb = chop bsize b

draw :: BoardMove -> Board -> Bool
draw bm b = not (any (isNothing . bMove) b)
         && not (win bm b)
~~~~~

Finally, we have the `gameloop` that ties all the game bits together including the user IO actions.

~~~~~{.haskell}
gameloop prevMove board = do
  let currPlayer = maybe X (\(BoardMove mv _) -> 
                               case mv of
                                 Just X -> O
                                 Just O -> X) prevMove
  putStr $ "Player '" ++ (show currPlayer) ++ "': "
  hFlush stdout
  playerMove <- getLine
  case (playerMove, (map toUpper playerMove) `elem` allCoord) of
    ("quit", _) ->
        putStrLn "Goodbye!"
    (_, False)  -> do
        putStrLn $ "Possible options: " ++ intercalate ", " allCoord
        gameloop prevMove board
    otherwise   -> do
        let pos = (toUpper $ playerMove !! 0, 
                   read [(playerMove !! 1)] :: Int)
            currMove = BoardMove (Just currPlayer) pos
            currBoard = move currMove board
        either putStrLn (putStrLn . (\s->"\n"++s++"\n") . printBoard) currBoard
    case currBoard of
          Right r  -> if win currMove r
                        then do putStrLn $ "Player '"
                                           ++ (show currPlayer) ++"' wins!"
                                main
                        else if draw currMove r
                                then do putStrLn $ "It's a draw!"
                                        main
                                else gameloop (Just currMove) r
          Left err -> gameloop prevMove board
  where allCoord = [[x] ++ show y | x <- take bsize (fst coord), 
                                    y <- take bsize (snd coord)]
~~~~~

For questions and feedback, you can drop me an email or a tweet.

**Full source:** [GitHub link](https://gist.github.com/3071732/).

* * *

[1] Why Functional Programming Matters - [http://www.cse.chalmers.se/~rjmh/Papers/whyfp.html](http://www.cse.chalmers.se/~rjmh/Papers/whyfp.html)