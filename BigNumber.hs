{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module BigNumber where -- TODO Change this
import Data.Char(digitToInt,ord,chr,isDigit)

-- ====================================== 2.1 ====================================== 
data BigNumberSign = Positive | Negative deriving (Enum, Eq, Show)
type BigNumberDigits = [Int]
data BigNumber = BigNumber BigNumberDigits  BigNumberSign deriving (Show)

-- =============================== Utility Functions =============================== 

-- Remove leading zeros from BigNumberDigits, e.g 000010 ([0,1,0,0,0,0]) -> 10 ([0,1])
removeLeadingZerosBN :: BigNumberDigits -> BigNumberDigits -- TODO: I think this could be more efficient
removeLeadingZerosBN [] = []
removeLeadingZerosBN l = if result /= [] then result else [0]
    where result = reverse (dropWhile (== 0) (reverse l))

-- Zip lists with different lenghts without slicing the longest one, instead, 
-- a default value will be added as padding to the shorter list.
zipWithDefaultValue :: [a] -> [a] -> a -> [(a,a)]
zipWithDefaultValue l1 l2 def = zip lpadded1 lpadded2
    where
          (len1, len2) = (length l1, length l2)
          padding = replicate (abs (len1 - len2)) def
          lpadded1 = if len1 > len2 then l1 else l1 ++ padding
          lpadded2 = if len2 > len1 then l2 else l2 ++ padding

greaterBNDigits :: BigNumberDigits -> BigNumberDigits -> Bool
-- Check if a BigNumberDigits if greater than another
greaterBNDigits d1 d2 = (diff /= []) && uncurry (>) (head diff)
    where diff = dropWhile (uncurry (==)) (reverse (zipWithDefaultValue d1 d2 0))
-- diff = Remove least significant numbers from both BN while they're equal, leaving only the difference.

greatereqBNDigits :: BigNumberDigits -> BigNumberDigits -> Bool
greatereqBNDigits d1 d2 = d1 == d2 || greaterBNDigits d1 d2

smallereqBNDigits :: BigNumberDigits -> BigNumberDigits -> Bool
smallereqBNDigits d1 d2 = not (greaterBNDigits d1 d2)

-- Utility function to get BigNumberDigits from a BigNumber
getBNDigits :: BigNumber -> BigNumberDigits
getBNDigits (BigNumber d _) = d

-- ====================================== 2.2 ====================================== 
decDigitToInt :: Char -> Int
decDigitToInt c -- digitToInt accepts hex digits [a,b,c,d,e,f] which we don't want.
    | isDigit c = digitToInt c
    | otherwise = error "invalid digit"

-- Scan a String and convert it to a BigNumber using decDigitToInt.
-- The result is reversed so the least significant numbers are at the head of 
--the list to facilitate operations.
scanner :: String -> BigNumber
scanner [] = error "invalid string"
scanner (h:str)
    | h == '-' && not (null str)  = BigNumber (reverse (map decDigitToInt str)) Negative
    | otherwise = BigNumber (reverse (map decDigitToInt (h:str))) Positive


-- ====================================== 2.3 ====================================== 
-- Convert a single digit, with a Int type, to it's corresponding Char
intToChar :: Int -> Char
intToChar x
    | x >= 0 && x <= 9 = chr(ord '0' + x)
    | otherwise = error "x should be in the range [0,9]"

-- Convert a BigNumber to a String
output :: BigNumber -> String
output (BigNumber digits sign) = if sign == Negative then '-' : string else string
    where string = reverse (map intToChar digits)

-- TODO: Make this description better
-- This function takes as an argument a
--      - function (Int -> Int -> Int -> (Int, Int)) that takes two digits and a carry, returning a digit and a carry
--          E.g for 9 + 5 the function would take (9 -> 5 -> 0) = (9+5 `mod` 10 = 4, 9+5 `div` 10 = 1)
--      - Two BigNumberDigits to which the function will be applied
--      - The initial carry (usually 0)
-- And returns new BigNumberDigits after the function has been applied
operationWithCarry :: (Int -> Int -> Int -> (Int, Int)) -> BigNumberDigits -> BigNumberDigits -> Int -> BigNumberDigits
operationWithCarry func [] [] c = [c | c /= 0] -- Return [carry] if it's different than 0
operationWithCarry func (d1:ds1) [] carry
    | carry == 0 = d1:ds1 -- If carry is 0 then the number won't be changed anymore.
    | otherwise = digit : operationWithCarry func [] ds1 new_carry
    where (digit,new_carry) = func d1 0 carry

operationWithCarry func [] (d2:ds2) carry
    | carry == 0 = d2:ds2
    | otherwise =  digit : operationWithCarry func [] ds2 new_carry
    where (digit,new_carry)  = func 0 d2 carry

operationWithCarry func (d1:ds1) (d2:ds2) carry = digit : operationWithCarry func ds1 ds2 new_carry
    where (digit,new_carry) = func d1 d2 carry

-- ====================================== 2.4 ====================================== 
-- somaBN will handle only numbers with the same sign, and delegate operations 
-- with different sign to subBN using Pattern Matching.
somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN (BigNumber a Positive) (BigNumber b Negative) = subBN (BigNumber a Positive) (BigNumber b Positive)
somaBN (BigNumber a Negative) (BigNumber b Positive) = subBN (BigNumber b Positive) (BigNumber a Positive)
somaBN (BigNumber d1 s1) (BigNumber d2 s2) = BigNumber (operationWithCarry somaOp d1 d2 0) s1 -- Both numbers have same sign due to pattern matching

-- Sums two digits (+carry), returning the correct digit (%10) and the resulting carry
somaOp :: Int -> Int -> Int -> (Int, Int)
somaOp x y carry = ((x+y+carry) `mod` 10, (x+y+carry) `div` 10)

-- ====================================== 2.5 ====================================== 
-- subBN will delegate operations to somaBN if the numbers have different sign
-- the remaining cases will be reduced into a subtraction 
-- between numbers A and B, A >= B.
subBN :: BigNumber -> BigNumber -> BigNumber
subBN (BigNumber d1 Negative) (BigNumber d2 Negative) = subBN (BigNumber d2 Positive) (BigNumber d1 Positive)
subBN (BigNumber d1 Positive) (BigNumber d2 Negative) = somaBN (BigNumber d2 Positive) (BigNumber d1 Positive)
subBN (BigNumber d1 Negative) (BigNumber d2 Positive) = somaBN (BigNumber d2 Negative) (BigNumber d1 Negative)
subBN (BigNumber d1 Positive) (BigNumber d2 Positive)
    | greatereqBNDigits d1 d2 = BigNumber (removeLeadingZerosBN(operationWithCarry subOp d1 d2 0)) Positive
    | otherwise = BigNumber (removeLeadingZerosBN(operationWithCarry subOp d2 d1 0)) Negative

-- Subtracts two digits (+carry), and returns the correct digit (%10) and the resulting carry
subOp :: Int -> Int -> Int -> (Int, Int)
subOp x y carry = ((x-y-carry) `mod` 10, if y+carry > x then 1 else 0)

-- ====================================== 2.6 ====================================== 
mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN (BigNumber [0] _) (BigNumber _ _) = BigNumber [0] Positive -- Small optimization, there's no need to calculate multiplications by 0
mulBN (BigNumber _ _) (BigNumber [0] _) = BigNumber [0] Positive

-- mulBN will multiply the first BigNumberDigits by each of the digits of the second BigNumber, one by one
-- And shift each successive result by adding zeros to the start of the number.
-- After all these multiplications have been completed they'll be added using somaBN
mulBN (BigNumber d1 s1) (BigNumber d2 s2) = foldr1 somaBN [BigNumber (replicate i 0 ++ mulBNDigitsbyInt d1 x) sign | (x,i) <- zip d2 [0..]]
    where sign = if s1 /= s2 then Negative else Positive

-- Multiply a BigNumber's digits byte a single unsigned Int. TODO: Maybe add a check to make sure is unsigned?
mulBNDigitsbyInt :: BigNumberDigits -> Int -> BigNumberDigits
mulBNDigitsbyInt d n = operationWithCarry mulOp d (replicate (length d) n) 0

-- Multiplies two digits (+carry), and returns the correct digit (%10) and the resulting carry
mulOp :: Int -> Int -> Int -> (Int, Int)
mulOp x y carry = ((x*y+carry) `mod` 10, (x*y+carry) `div` 10)

-- ====================================== 2.7 ====================================== 

-- Utility functions that take BigNumberDigits instead of BigNumbers
subBNDigits :: BigNumberDigits -> BigNumberDigits -> BigNumberDigits -- Assuming positive numbers
subBNDigits d1 d2 = getBNDigits (subBN (BigNumber d1 Positive) (BigNumber d2 Positive))

mulBNDigits :: BigNumberDigits -> BigNumberDigits -> BigNumberDigits -- Assuming positive numbers
mulBNDigits d1 d2 = getBNDigits (mulBN (BigNumber d1 Positive) (BigNumber d2 Positive))

divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN (BigNumber [0] Positive) (BigNumber _ Positive) = (BigNumber [0] Positive, BigNumber [0] Positive)
divBN (BigNumber _ Positive) (BigNumber [0] Positive) = error "Division by 0"
divBN (BigNumber ds1 Positive) (BigNumber [1] Positive) = (BigNumber ds1 Positive, BigNumber [0] Positive)
divBN (BigNumber ds1 Positive) (BigNumber d2 Positive) = (BigNumber (removeLeadingZerosBN d) Positive, BigNumber (removeLeadingZerosBN r) Positive)
    where (d,r) = divBNAux (init ds1) [last ds1] d2 []

-- Take largest multiple of divisor (by multiplying from 1 to 9) that fits in dividend
-- Returns the dividend - largestMultiple and the multiplier.
smallDivide :: [Int] -> [Int] -> (BigNumberDigits, Int)
smallDivide dividend divisor = (subbed_number, m)
      where
        subbed_number = subBNDigits dividend biggestMultiple
        (biggestMultiple, m) = last (takeWhile (\(x, y) -> smallereqBNDigits x dividend) [(mulBNDigits divisor [i], i) | i <- [1..9]])

divBNAux :: BigNumberDigits -> BigNumberDigits -> BigNumberDigits -> BigNumberDigits -> (BigNumberDigits, BigNumberDigits)
divBNAux [] dividend_r divisor quocient
    | greaterBNDigits divisor dividend_r = (0:quocient, dividend_r)
    | otherwise = (m:quocient, subNumber)
    where (subNumber, m) = smallDivide dividend_r divisor

divBNAux dividend_l dividend_r divisor quocient
    | greaterBNDigits divisor dividend_r = divBNAux (init dividend_l) (last dividend_l:dividend_r) divisor (0:quocient)
    | otherwise = divBNAux (init dividend_l) (last dividend_l : subNumber) divisor (m:quocient)
    where (subNumber, m) = smallDivide dividend_r divisor

-- ==================================  5.0  ====================================== 
safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN bn1 (BigNumber d2 s2)
    | d2 == [0] = Nothing
    | otherwise = Just (divBN bn1 (BigNumber d2 s2))