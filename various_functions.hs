-- Tim Leavey
-- Haskell often uses pattern matching, recusion, and list comprehension.
-- Haskell actually should be written in camelCase, not snake_case, so I
-- should refactor this at some point.

import Data.Char

-- Step Function
step :: (Num a, Ord a, Fractional p) => a -> p
step x
    | (x < 0) = 0
    | (x == 0) = 0.5
    | (x > 0) = 1

-- Sigmoid Function
sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + e ** (-1 * x))
    where
    e = exp 1

-- Encode Function: Encodes a capital letter a by adding b to ASCII value.
-- This encode function can handle all integers, even those above 26 or less than -26.
-- Getting it to loop multiple times, using mod, took a bit to figure out.
encode :: Char -> Int -> Char
encode a b
    | (isAsciiUpper(a) == True) && (asciiValue >= 65 && asciiValue <= 90) && (theTotalAsciiValue < 65) && (not (newTotalAsciiValueLow == 91)) = theFinalCharLow
    | (isAsciiUpper(a) == True) && (asciiValue >= 65 && asciiValue <= 90) && (theTotalAsciiValue < 65) && (newTotalAsciiValueLow == 91) = 'A'
    | (isAsciiUpper(a) == True) && (asciiValue >= 65 && asciiValue <= 90) && (theTotalAsciiValue > 90) && (not (newTotalAsciiValueHigh == 64)) = theFinalCharHigh
    | (isAsciiUpper(a) == True) && (asciiValue >= 65 && asciiValue <= 90) && (theTotalAsciiValue > 90) && (newTotalAsciiValueHigh == 64) = 'Z'
    | (isAsciiUpper(a) == True) && (asciiValue >= 65 && asciiValue <= 90) && (theTotalAsciiValue <= 90 && theTotalAsciiValue >= 65) = theFinalChar
    | otherwise = '!'
    where
        asciiValue = ord(a)
        theTotalAsciiValue = asciiValue + b
        differenceFromCharA = 65 - theTotalAsciiValue 
        newTotalAsciiValueLow = 91 - (differenceFromCharA `mod` 26)
        theFinalCharLow = chr(newTotalAsciiValueLow)
        differenceFromCharZ = theTotalAsciiValue - 90
        newTotalAsciiValueHigh = 64 + (differenceFromCharZ `mod` 26)
        theFinalCharHigh = chr(newTotalAsciiValueHigh)
        theFinalChar = chr(theTotalAsciiValue)

-- Decodes a capital letter a by subracting b from a's ASCII value.
-- Decode can handle all integers as well.
decode :: Char -> Int -> Char
decode a b
    | (isAsciiUpper(a) == True) && (asciiValue >= 65 && asciiValue <= 90) && (theTotalAsciiValue < 65) && (not (newTotalAsciiValueLow == 91)) = theFinalCharLow
    | (isAsciiUpper(a) == True) && (asciiValue >= 65 && asciiValue <= 90) && (theTotalAsciiValue < 65) && (newTotalAsciiValueLow == 91) = 'A'
    | (isAsciiUpper(a) == True) && (asciiValue >= 65 && asciiValue <= 90) && (theTotalAsciiValue > 90) && (not (newTotalAsciiValueHigh == 64)) = theFinalCharHigh
    | (isAsciiUpper(a) == True) && (asciiValue >= 65 && asciiValue <= 90) && (theTotalAsciiValue > 90) && (newTotalAsciiValueHigh == 64) = 'Z'
    | (isAsciiUpper(a) == True) && (asciiValue >= 65 && asciiValue <= 90) && (theTotalAsciiValue <= 90 && theTotalAsciiValue >= 65) = theFinalChar
    | otherwise = '!'
    where
        asciiValue = ord(a)
        theTotalAsciiValue = asciiValue - b
        differenceFromCharA = 65 - theTotalAsciiValue 
        newTotalAsciiValueLow = 91 - (differenceFromCharA `mod` 26)
        theFinalCharLow = chr(newTotalAsciiValueLow)
        differenceFromCharZ = theTotalAsciiValue - 90
        newTotalAsciiValueHigh = 64 + (differenceFromCharZ `mod` 26)
        theFinalCharHigh = chr(newTotalAsciiValueHigh)
        theFinalChar = chr(theTotalAsciiValue)

-- Returns True if encode(c,x) equals c'
-- Returns False if encode(c,x) does not equal c'
check :: Char -> Int -> Char -> Bool
check c x z
    | (encode c x == z) = True
    | otherwise = False

-- Append to end of list
append :: [a] -> a -> [a]
append lst item = lst ++ [item]

-- Bookends a number to both ends of list
bookends :: [a] -> a -> [a]
bookends lst item =
    item:lst ++ [item]

-- Returns the last element in a list
my_last :: [a] -> a
my_last [lst] = lst
my_last (_:lst) = my_last lst
my_last [] = error "No last number in an empty list!"

-- Returns True or False depending if a leap year was entered
is_leap :: Integral a => a -> Bool
is_leap year =
    if (year `mod` 4 == 0) && not (year `mod` 100 == 0)
        then True
    else
        if (year `mod` 100 == 0) && not (year `mod` 400 == 0) then False
        else if year `mod` 400 == 0 then True
        else False

-- Returns acronym from first letter of each word in string
acronym :: String -> String
acronym phrase =
    let list_of_words = words phrase
        all_caps = map (map toUpper) list_of_words
        first_letter_of_first_word = map head all_caps
    in first_letter_of_first_word

-- Takes two points and adds them together
padd :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
padd (x1,y1) (x2,y2) =
    (x1 + x2, y1 + y2)

--Tells the user if input is higher than, lower than, or the right the secret number
secret_number = 2
guess_num :: Integer -> IO ()
guess_num x =
    if x > secret_number then putStrLn "too high"
    else
        if x < secret_number then putStrLn "too low"
        else putStrLn "got it!"

-- Planet age takes a planet and age and returns equivalent age on given planet
planet_age :: Fractional a => [Char] -> a -> a
planet_age planet age
    | planet == "Mercury" = (age / 0.241)
    | planet == "Venus" = (age / 0.615)
    | planet == "Earth" = (age / 1.000)
    | planet == "Mars" = (age / 1.881)
    | planet == "Jupiter" = (age / 11.863)
    | planet == "Saturn" = (age / 29.447)
    | planet == "Uranus" = (age / 84.017)
    | planet == "Neptune" = (age / 164.791)

-- Prompts user    for input to    encrypt 
-- • Reads a   line    of  user    input
-- • Encodes   the input   string somehow  
-- • Prints    encrypted   message
do_encrypt_v2 :: IO ()
do_encrypt_v2 = do
    putStrLn "Enter phrase to encrypt: "
    (x:xs) <- getLine
    let original_message = (x:xs)
    let ascii_valued_list = map ord(x:xs)
    let new_ascii_value = map add_one ascii_valued_list
    let encrypted_message = map ascii_to_char new_ascii_value
    putStrLn encrypted_message

-- These 2 functions are helper functions for do_encrypt_v2
-- add_one adds 1 to an Int
add_one :: Num a => a -> a
add_one x = x + 1
-- Converts an ascii value to a char
ascii_to_char :: Int -> Char
ascii_to_char x = chr(x)

-- Encryption using list comprehension
do_encrypt :: IO ()
do_encrypt = do
    putStrLn "Enter phrase to encrypt: "
    input <- getLine
    putStrLn [ chr ((ord i) + 1) | i <- input]

-- Decrypts a user's input
do_decrypt :: IO ()
do_decrypt = do
    putStrLn "Enter phrase to decrypt: "
    input <- getLine
    putStrLn [ chr ((ord i) - 1) | i <- input]

-- Performs a linear search through a list and returns whether the target element was found or not found
linear :: Eq t => t -> [t] -> [Char]
linear item (x:xs)
    | item == x = "found"
    | xs == [] = "not found"
    | otherwise = linear item xs

-- Takes a list and a number(n), returns first n element in list 
my_first :: [a] -> Int -> [a]
my_first lst n = do
    let reversed_list = reverse lst
    let elements_i_want = drop (length reversed_list - n) reversed_list 
    reverse elements_i_want

-- Takes a list and a number(n), returns last n element in list 
my_last :: [a] -> Int -> [a]
my_last lst n = do
    drop (length lst - n) lst

-- Takes a list, divides it in half, and returns a tuple containing the 1st and 2nd halves
my_split :: [a] -> ([a], [a])
my_split lst = do
    let index = length lst `div` 2
    if odd (length lst)
        then    
        let first_half = my_first lst (index + 1)
            second_half = my_last lst index
        in (first_half, second_half)
    else
        let first_half = my_first lst index
            second_half = my_last lst index
        in (first_half, second_half)        

-- Takes a list and returns a list of values at even element locations.
evens :: [a] -> [a]
evens [] = []
evens [x] = x:[]
evens (x:y:xs) = x:evens xs


-- Takes a list and returns a list of values at odd element locations.
odds :: [a] -> [a]
odds [] = []
odds [x] = []
odds [x,y] = y:[]
odds (x:y:xs) = y:odds xs


-- Krle takes a list of ints and an int, then returns a list of tuples of ints.
krle :: [Int] -> Int -> [(Int, Int)]
krle lst k = list_comp (make_tuple lst) k

-- Compare_two checks to see if a value falls within the range of 
-- the original value that is being checked against.
-- It returns True or False depending on if it falls within range.
compare_two :: (Ord a, Num a) => a -> a -> a -> Bool
compare_two x y k
    | (y >= x - k) && (y <= x + k) = True
    | otherwise = False

-- Make_tuple turns a list of numbers into a list of tupled pairs (x,y).
-- All y-values are set with 1 initially.
make_tuple :: Num b => [a] -> [(a, b)]
make_tuple (x:[]) = (x,1) : []
make_tuple (x:xs) = (x,1) : make_tuple xs

-- List_comp takes a list of tupled pairs of ints and an int, then returns a list of tupled pairs of ints.
-- first_value is the first value (x) in the first pair in the list.
-- first_value gets checked against in compare_two during list comprehension.
-- grps uses list comprehension to set pairs of tuples if they meet the condition, then
-- the length of grps is taken and put as a second value (y) into one_pair.
-- The base case checks for an empty list, and the recursive call goes through all the tupled pairs.
list_comp :: [(Int, Int)] -> Int -> [(Int,Int)]
list_comp my_list k = do
    let first_value = fst (head my_list)
    let grps = [ (a,b) | (a,b) <- my_list, compare_two first_value a k ]
    let one_pair = (first_value, length grps)
    if my_list == [] then
        []
    else
        one_pair : list_comp (drop (length grps) my_list) k

-- Krld takes a list of tupled pairs of ints and decompresses it as a list of numbers.
-- Using list comprehension, x is repeated y times.
-- The result is flattened using concat since the list comprehension makes it a list of lists.
krld :: [(a, Int)] -> [a]
krld lst = do
    let result = [ take y (repeat x) | (x,y) <- lst ]
    let flat_result = concat result
    flat_result