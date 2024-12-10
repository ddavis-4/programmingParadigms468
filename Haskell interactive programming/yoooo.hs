putStr' :: String -> IO()
putStr' [] = return ()
putStr' (x:xs) = sequence_ [putChar x, putStr' xs]

--putStr' xs = squence_ [putChar x|x <- xs]

main :: IO ()
main = do 
    putStr' "Enter Password: "
    xs <- getLine
    putStr' $ "Password: " ++ ['*' | _ <-[1..length xs]]
    putStr' "\n"
    putStr' (show (length xs))
    putStr' "\n"
    
