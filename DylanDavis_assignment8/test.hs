{- Dylan Davis
3047302
EECS 468
11/24/2023
Assignment 8 haskell parser 
Collaborators: chat gpt, Ashley Aldave-}
import System.IO (hFlush, stdout) -- imports  

-- this block of code defines "book", to be a custom data type. 
data Book = BNumber Double -- creates constructor, representing a double number value   
           | BOperator String --  As above create consturctor, represeting a string value 
           | BLParen -- ^^, representing the left paranthesis
           | BRParen -- ^^r, book representing the right paranthesis
           deriving (Show, Eq) -- this line derives show & eq which allow for book to be displayed and compared

book__ :: String -> Either String [Book] --defines book__ a function that takes a string and or a list of our custome data type book
book__ [] = Right [] -- returns an empty list when book__ is empty.
book__ (c:cs) -- pattern matcheing for recieved strings that are non empty, c represents first character in the string, cs represents rest.
    | c `elem` "+-*/%()" = -- makes sure the first character is one of the specified arithmetic operators: +, -, *, /, %, or parentheses ().
      if c == '*' && cs /= [] && head cs == '*' -- if statement for exponentiational. 
        then case book__ (drop 2 cs) of -- recursively call book__ after ** exponentiation 
            Left err -> Left err -- if error occurs in recursion, propagate error
            Right books -> Right (BOperator "**" : books) -- if successful, '**' operator add book to the book list 
        else case book__ cs of -- for other operators, recursively call book__ the rest of the string 
            Left err -> Left err -- if error, then propagates error
            Right books -> Right (BOperator [c] : books) -- adds operator book to the books list
    | c == ' ' = book__ cs -- if charcters is a space, recursivly call book__ on the rest of string 
    | c `elem` ['0'..'9'] || c == '.' = -- if the character is a digit or dot then execute the following 
        let (n, r) = span (\x -> x `elem` ['0'..'9'] || x == '.') (c:cs) -- this line of code splits the input list into two parts where 'n' contains all consecutive digits(dots), 'r'(remainder).
        in case book__ r of -- matches patterns based on result of running book__ the 'r' part of the input string
            Left err -> Left err -- if error occurs in running book__, then propagate error
            Right books -> case (reads n :: [(Double, String)]) of -- parses the number and continues to book__ the remaining string
                    [(n, "")] -> Right (BNumber n : books) -- if there is no error, adds is as book to list 
                    _ -> Left $ "Invalid number format: " ++ n -- raise error is the format isn't valid
    | c == '^' = Left "Invalid character: ^" -- raise character error for ^
    | otherwise = Left $ "Invalid character: " ++ [c] -- raise character error for any invalid character  

data Express = -- this line defines a data type Express to rep an expression
          Num Double -- creates constructor for a numeric value 
          | BinOp String Express Express -- this creates a constructor for binary operation, and stores as 2 sub expressions 
          deriving Show -- derives 'show' which allow book to be displayed 

--the primary purpose of this function is to get the primary expression from a list 
parsePrimary :: [Book] -> Either String (Express, [Book])
parsePrimary [] = Left "Unexpected end of expression" -- error if list is empty
parsePrimary (BNumber x : ts) = Right (Num x, ts) -- succsesfully parse and return an expression
parsePrimary (BOperator "(" : rest) = do
    (express, afterExpr) <- parseAddSub rest -- parse parenthesis
    case afterExpr of
        -- in the case of closed parenthesis return the expression 
        (BOperator ")" : remainingBooks) -> Right (express, remainingBooks)
        _ -> Left "Unmatched parentheses" -- error if the parenthesis are left unclosed
parsePrimary (BOperator op : _) | op `elem` ["+", "-", "*", "/", "%", "**"] =
    Left $ "Missing Operand" -- error if operand is missing 
parsePrimary (BOperator _ : ts) = Left "Missing Operator" -- error if unexpected operand is encountered 
parsePrimary _ = Left "Invalid expression" -- error raised for any other invalid expression
          

parseExpr :: [Book] -> Either String Express -- this line of code defines parseExpr a function that parses a list of books into expressions and returns either an expression or error message
parseExpr books = do -- parses list of books into expression, and also checks for missing operators
    (express, remainingBooks) <- parseAddSub books -- this parses the expressions using parseAddSub, checks for any remianing books
    checkMissingOperator remainingBooks -- this line calls checkMissingOperator and also checks for missing operators in the remaining books
    if null remainingBooks -- if there are no remaining books, then return the expression after parsing else an error is raised
        then return express -- return the parsed expression
        else Left "Invalid expression: Trailing Books" -- return Invalid expression: Trailing books expression

checkMissingOperator :: [Book] -> Either String () -- this defines the checkMissingOperator function, which checks for missing operators returns an error message if operator is missing. 
checkMissingOperator [] = Right () -- if the list is empty or the book is valid, return succesfully, and display no error
checkMissingOperator [BNumber _] = Right () -- if there is a single BNumber book that has no missing operators, return success
checkMissingOperator [BLParen] = Right () -- if single BLParen Book has no missing operators, then returns success
checkMissingOperator (BNumber _ : BNumber _ : _) = -- this checks if operator is absent between two numbers, raise error if absent
    Left "Missing Operator" -- returns missing operator error 
checkMissingOperator (BNumber _ : BLParen : _) = -- this checks if operator is missing between number and left parenthesis, if so gives missing operator error 
    Left "Missing Operator" -- returns missing operator error 
checkMissingOperator (BRParen : BNumber _ : _) = -- this checks if operator is missing between right parenthesis and number, if so gives missing operator error
    Left "Missing Operator" -- returns missing operator error
checkMissingOperator (BRParen : BLParen : _) = -- checks if operator is missing between right parenthesis and left parenthesis , if so gives missing operator error
    Left "Missing Operator" -- returns missing operator error
checkMissingOperator (BRParen : BOperator _ : _) = -- this checks that right parenthesis is following by and operator book indicating that it could potentially be missing an operator, so it raises the error 
    Left "Missing Operator" -- returns missing operator error
checkMissingOperator (BOperator _ : BNumber _ : _) = -- checks if operator book and if is followed by number book,like above , if so gives missing operator error 
    Left "Missing Operator" -- returns missing operator error
checkMissingOperator (BOperator _ : BLParen : _) = -- this checks if operator book is followed by left parenthesis, and same as above operator error
    Left "Missing Operator" -- returns missing operator error
checkMissingOperator (BOperator _ : BRParen : _) = -- checks if operator book is followed by right parenthesis, this like above would indicate a missing operator
    Left "Missing Operator" -- returns missing operator error
checkMissingOperator (BOperator _ : BOperator _ : _) = -- same as above 
    Left "Missing Operator" -- returns missing operator error
checkMissingOperator _ = Right () -- if no missing opertor error found, then returns success

parseAddSub :: [Book] -> Either String (Express, [Book]) -- this defines the parseAddSub function, it parses addition and subtraction operation, either reurns an expression or error
parseAddSub books = do -- this initiates a do block for parsing addition and subtraction operations from list of books
    (express, remainingBooks) <- parseMulDivMod books -- this gets the expression and remaining books after parsing multiplication, division, and modulo ops
    parseAddSub' express remainingBooks -- parses addition and subtraction operations with obtained expression/remaining books

parseAddSub' :: Express -> [Book] -> Either String (Express, [Book]) -- this defines the parseAddSub' function, parses addition/subtraction ops w/ expression , returns new expression, remaining books, or error is raised
parseAddSub' express [] = Right (express, []) -- when there are no more books, it returns an empty list and the current expression
parseAddSub' express (BOperator op : books) -- parses based on the operator book thats is found
    | op `elem` ["+", "-"] = do -- do block initiated if the specified operators are present in the list 
        (expr2, remainingBooks) <- parseMulDivMod books  -- parses multiplication, division, and modulo operations to get new expression and remaining books
        let newExpr = BinOp op express expr2 -- this creates a new expression and applies the operator between two sub-expressions
        parseAddSub' newExpr remainingBooks -- and this parses the new expression
    | otherwise = Right (express, BOperator op : books) -- returns the current expression and operator book if its neither addition nor subtraction
    
parseMulDivMod :: [Book] -> Either String (Express, [Book]) -- this defines parseMulDivMod function, this parses multiplication, division, and modulo ops from a list of books, and like before will return remaining books or an error
parseMulDivMod books = do -- initiates a do block for parsing multiplication, division and modulo operations from list of books
    (express, remainingBooks) <- parseExponent books -- gets the expression and remaining books after parsing exponent operation
    parseMulDivMod' express remainingBooks -- this parses multiplication, division, and modulo operators with the expression/remaining books

parseMulDivMod' :: Express -> [Book] -> Either String (Express, [Book]) -- this defines parseMilDivMod' function which parses multiplication, division, and modulo with an expression or list of books, it will return new expression the remaining books or an error will be raised
parseMulDivMod' expr [] = Right (expr, []) -- when there are no more books,it returns the current expression 
parseMulDivMod' expr (BOperator op : books) -- parse base on the operator book found
  | op `elem` ["*", "/", "%"] = do -- do block is initiated if the specified operators are present in the list
    (expr2, remainingBooks) <- parseExponent books -- parses exponent operation, gets new expression/remaining book
    let newExpr = BinOp op expr expr2 -- creates new expression applying the operator to two sub-expressions
    parseMulDivMod' newExpr remainingBooks -- parses the new expression
  | otherwise = Right (expr, BOperator op : books) -- returns current expression and operator book if it's neither multiplication, division, or modulo
parseMulDivMod' expr (BNumber n : books) = -- case for error, gives an error when number book without and operator found
  Left "Missing Operator" -- returns missing operator error message

parseExponent :: [Book] -> Either String (Express, [Book]) -- this parses the exponent operations, and returns the expression or the remianing books else an error is rasied. 
parseExponent books = do -- - initiates do block for parsing exponent operations from list of bookss
    (expr, remainingBooks) <- parsePrimary books --  gets expression and remaining bookss after parsing 
    parseExponent' expr remainingBooks -- parses the exponent operations with the expression or remaining books

-- this is a function to parse exponent operations with remaining books
-- Takes an expression (Express) and a list of Books ([Books])
-- and it returns either an error message (Left) or a tuple containing a new expression and remaining books (Right)    
parseExponent' :: Express -> [Book] -> Either String (Express, [Book])
parseExponent' expr [] = Right (expr, []) -- no more books return the current expression 
parseExponent' expr (BOperator "**" : books) = do -- if list start with **
    (expr2, remainingBooks) <- parsePrimary books -- parse the first expression after **
    let newExpr = BinOp "**" expr expr2 -- create a new expression with the ** operator
    parseExponent' newExpr remainingBooks -- recursivly parse the rest
parseExponent' expr books = Right (expr, books) -- if the list does not start with ** do nothing.

evaluate :: Express -> Either String Double
evaluate (Num x) = Right x -- if the expression is a number then return the value 
evaluate (BinOp "**" expr1 expr2) = do -- If the expression is an exponentiation operation
    x <- evaluate expr1 -- evaluate sub expression 1
    y <- evaluate expr2 -- evaluate sub expression 2
    return (x ** y) -- return result
evaluate (BinOp op expr1 expr2) = do -- if a binary operation 
    x <- evaluate expr1 -- evaluate sub expression 1 once again 
    y <- evaluate expr2 -- 2 once again 
    case op of
        "+" -> return (x + y) -- if addition return sum 
        "-" -> return (x - y) -- if sum return difference
        "*" -> return (x * y) -- product for multiplication 
        "/" -> if y /= 0 then return (x / y) else Left "Incorrect Operator Usage" -- division
        "%" -> if y /= 0 then return (x - fromIntegral (truncate (x / y)) * y) else Left "Invalid Operator Usage" -- modulo
        _ -> Left "Invalid operator"

parse :: String -> Either String Double
parse input = do -- start a do block for composition
    books <- book__ input -- book__ input string giving a list of books
    express <- parseExpr books -- parse list of books into an expression
    evaluate express

main :: IO ()
main = do
  putStrLn "Enter an arithmetic expression (or type 'exit' to quit):"
  userInput <- getLine
  if userInput /= "exit"
    then do
      let result = parse userInput
      case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right res -> putStrLn $ "Result: " ++ show res
      main
    else putStrLn "Exiting the program."
