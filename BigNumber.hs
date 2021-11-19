{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module BigNumber where -- TODO Change this
import Data.Char(digitToInt,ord,chr)

-- 2.1
data BigNumberSign = Positive | Negative deriving (Enum, Eq, Show)
type BigNumberDigits = [Int]
data BigNumber = BigNumber BigNumberDigits  BigNumberSign deriving (Show)

-- 2.2
-- TODO: Throw error if chr is out of range ['0':'9']?
-- Scan a String and convert it to a BigNumber
scanner :: String -> BigNumber
scanner [] = error "invalid string"
scanner (h:str)
    | h == '-'  = BigNumber (reverse (map digitToInt str))     Negative
    | otherwise = BigNumber (reverse (map digitToInt (h:str))) Positive


-- 2.3
-- Convert a single digit, with a Int type, to it's corresponding Char
intToChar :: Int -> Char
intToChar x
    | x >= 0 && x <= 9 = chr(ord '0' + x)
    | otherwise = error "x should be in the range [0,9]"

-- Convert a BigNumber to a String
output :: BigNumber -> String
output (BigNumber digits sign) = if sign == Negative then '-' : string else string
    where string = reverse (map intToChar digits)

removeLeadingZerosBN :: BigNumberDigits -> BigNumberDigits -- TODO: I think this could be more efficient
removeLeadingZerosBN [] = []
removeLeadingZerosBN l = if result /= [] then result else [0]
    where result = reverse (dropWhile (== 0) (reverse l))


-- TODO: Remove the following message
-- I'm sorry for creating this monster. (Part of me wanted it to be a one-liner with lambdas, it would be glorious)

-- This function helps calculate any operation that requeries carry
-- For example, sum of [2,2] [9,1,1]. 
-- Firstly the numbers will be zipped (padding will be added): [(2,9), (2,1), (0,1)]
-- The scanl will start calculating:  func (0,0) (2,9) -> (11`mod`10 = 1 -> digit, 1 -> carry) 
--                                    func (_,1) (2,1) -> (2+1+1 = 4, 0)
--                                    func (_,0) (0,1) -> (1,0)
-- The list will look like this [(0,0), (1,1), (4,0), (1,0)] after removing the first element,
-- we take the 1st digit of each tuple [1, 4, 1]
idkWhatToNameThis :: ((Int,Int) -> (Int, Int) -> (Int, Int)) -> BigNumberDigits -> BigNumberDigits -> BigNumberDigits
idkWhatToNameThis func d1 d2 = removeLeadingZerosBN (map fst result ++ [last_carry]) -- Removing leading zeros is useless on (+) but this looks cleaner... what should we do?
    where result = tail (scanl func (0,0) (zip (d1++padding) (d2++padding)))
          last_carry = snd (last result)
          padding = replicate (abs (length d1 - length d2)) 0

-- 2.4
somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN (BigNumber a Positive) (BigNumber b Negative) = subBN (BigNumber a Positive) (BigNumber b Positive)
somaBN (BigNumber a Negative) (BigNumber b Positive) = subBN (BigNumber b Positive) (BigNumber a Positive)
somaBN (BigNumber d1 s1) (BigNumber d2 s2) = BigNumber (idkWhatToNameThis somaBNAux d1 d2) s1 -- Both numbers have same sign due to pattern matching

somaBNAux :: (Int, Int) -> (Int, Int) -> (Int, Int)
somaBNAux (_,carry) (x,y) = ((x+y+carry) `mod` 10, (x+y+carry) `div` 10)


-- 2.5
greaterBNDigits :: BigNumberDigits -> BigNumberDigits -> Bool 
greaterBNDigits d1 d2 = if length d1 == length d2 then reverse d1 > reverse d2 else length d1 > length d2 -- TODO: These two could be merged 

greatereqBNDigits :: BigNumberDigits -> BigNumberDigits -> Bool
greatereqBNDigits d1 d2 = if length d1 == length d2 then reverse d1 >= reverse d2 else length d1 > length d2

subBN :: BigNumber -> BigNumber -> BigNumber
subBN (BigNumber d1 Negative) (BigNumber d2 Negative) = subBN (BigNumber d2 Positive) (BigNumber d1 Positive)
subBN (BigNumber d1 Positive) (BigNumber d2 Negative) = somaBN (BigNumber d2 Positive) (BigNumber d1 Positive)
subBN (BigNumber d1 Negative) (BigNumber d2 Positive) = somaBN (BigNumber d2 Negative) (BigNumber d1 Negative)
subBN (BigNumber d1 Positive) (BigNumber d2 Positive)
    | greatereqBNDigits d1 d2 = BigNumber (idkWhatToNameThis subBNAux d1 d2) Positive -- TODO: This positive is temporary
    | otherwise = BigNumber (idkWhatToNameThis subBNAux d2 d1) Negative

subBNAux :: (Int, Int) -> (Int, Int) -> (Int, Int)
subBNAux (_, carry) (x, y) = ((x-y-carry) `mod` 10, if y+carry > x then 1 else 0)

-- 2.6
mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN (BigNumber [0] _) (BigNumber _ _) = BigNumber [0] Positive -- TODO: Say this is for efficiency reasons, in reality is to avoid 0*(-1) = -0. Or find a cleaner alternative :shrug:
mulBN (BigNumber _ _) (BigNumber [0] _) = BigNumber [0] Positive
mulBN (BigNumber d1 s1) (BigNumber d2 s2) = foldr1 somaBN [BigNumber (replicate i 0 ++ mulBNDigitsbyInt d1 x) sign | (x,i) <- zip d2 [0..]]
    where sign = if s1 /= s2 then Negative else Positive

mulBNDigitsbyInt :: BigNumberDigits -> Int -> BigNumberDigits  -- TODO: This is assuming unsigned Int!
mulBNDigitsbyInt d n = idkWhatToNameThis mulBNAux d (replicate (length d) n)

mulBNAux :: (Int, Int) -> (Int, Int) -> (Int, Int)
mulBNAux (_,carry) (x,y) = (((x*y)+carry) `mod` 10, ((x*y)+carry) `div` 10)