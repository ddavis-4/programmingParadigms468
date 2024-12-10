{--
Dylan Davis
3047302
EECS 468
11/13/2023
inclass problem
collaborators: chatgpt Ashley Aldave 
--}

fibs :: [Integer]
fibs = 0 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]

