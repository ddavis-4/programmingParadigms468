import System.IO

main :: IO ()
main = do
    putStr "Enter your password: "
    hFlush stdout  -- Flush the output buffer to ensure the prompt is displayed
    password <- getPassword
    putStrLn $ "\nPassword length: " ++ show (length password)

-- Function to read a password with asterisk echoing
getPassword :: IO String
getPassword = do
    password <- getPassword' ""
    putChar '\n'  -- Move to the next line after password entry
    return password

-- Helper function with recursion for password entry
getPassword' :: String -> IO String
getPassword' password = do
    hSetEcho stdin False  -- Turn off character echoing
    char <- getChar
    if char == '\n'
        then do
            hSetEcho stdin True  -- Turn character echoing back on
            return password
        else do
            putChar '*'  -- Echo an asterisk
            getPassword' (password ++ [char])
