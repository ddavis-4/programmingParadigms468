{--
Write a Haskell script called HelloWorld.hs that prints “Hello World!” in the following ways:
1. Use one putStrLn. Recall: putStrLn :: String -> IO () writes a string and moves to a new line.

2. Uses the do function, putStr, and putChar, with the two strings: “Hello” and “World!”. Recall: 

putStr ::String -> IO () 
writes a string without moving to a new line, and 
putChar :: Char -> IO () 
writes a single character to the screen.

3. Uses the do, let, concat, and putStr keywords, with the two strings: “Hello” and “World!\n”.
Recall: concat is a “pure” function that concatenates a list of strings.

--}
import System.IO

-- HelloWorld
main :: IO ()
main = putStrLn "Hello World!"

-- HelloWorld'
main' :: IO ()
main' = do
  putStr "Hello"
  putChar 'W'
  putChar 'o'
  putChar 'r'
  putChar 'l'
  putChar 'd'
  putStrLn "!"

-- HelloWorld
main'' :: IO ()
main'' = do
  let hello = "Hello"
      world = "World!\n"
      helloworld = concat [hello ++ world]

  putStr helloworld



