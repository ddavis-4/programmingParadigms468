{--
Dylan Davis
3047302
EECS 468
12/6/23
collaborators: stackoverflow, chatGPT, Ashley Aldave 
--}

import Data.Char (isDigit, digitToInt) -- import functions for inputs later 

type Board = [Int] -- define list of ints for the stars in each row

initial :: Board
initial = [5, 4, 3, 2, 1] -- defines the start board or the initial 

-- ==Function to display the board== --
displayBoard :: Board -> String 
displayBoard board = unlines [show i ++ ": " ++ replicate stars '*' | (i, stars) <- zip [1..] board]
{-- list comp to display a string rep of the board unlines is a function that concatenates the 
rows with new line character --}

-- ==Function to check if the game is finished== --
isGameOver :: Board -> Bool
isGameOver = all (== 0)

-- ==Function to check if a move is valid== --
isValidMove :: Board -> Int -> Int -> Bool
isValidMove board row starsToRemove =
  row >= 1 && row <= length board && starsToRemove >= 1 && starsToRemove <= board !! (row - 1) -- makes sure there are still stars in a row 

-- ==Function to update the board after a move== --
updateBoard :: Board -> Int -> Int -> Board
updateBoard board row starsToRemove =
  take (row - 1) board ++ [max 0 (board !! (row - 1) - starsToRemove)] ++ drop row board -- takes the stars to remove and updates the board 

-- ==Recursive main game loop== --
play :: Board -> Int -> IO () -- play function, where we use board from above 
play board currentPlayer = do 
  putStrLn $ displayBoard board -- calls displayBoard function and printing use putStrLn
  if isGameOver board -- if statment for win using game over function
    then putStrLn $ "Player " ++ show (3 - currentPlayer) ++ " wins!"
    else do -- else
      putStrLn $ "Player " ++ show currentPlayer -- show the current player
      putStr "Enter a row number: " -- prompt
      userRow <- getLine -- read user input
      starRemove <- getInput "Stars to remove: " -- get user input for star to remove 
      let row = read userRow -- convert input into ints
          starsToRemove = read starRemove -- bind to new variable 
      if isValidMove board row starsToRemove -- check if it is a valid move and then calls everything (the avengers assemble here)
        then play (updateBoard board row starsToRemove) (3 - currentPlayer) --  recursive call of the play function is called with updated board function as well 
        else do -- else
          putStrLn "ERROR: Invalid move" -- if the move is invalid error 
          play board currentPlayer -- the players/users move again 

-- ==Function to get input from the user, handling non-digit inputs== --
getInput :: String -> IO String
getInput prompt = do -- do statment
  putStr prompt -- prompt
  input <- getLine
  if all isDigit input -- if all digits are actually digits 
    then return input -- return it 
    else do 
      putStrLn "ERROR: Invalid input. Please enter a valid number." -- else return error
      getInput prompt -- prompt

-- ==Main function to start the game== --
nim :: IO () -- starts the game 
nim = play initial 1 --call play func with the initial board and play 1 up 
