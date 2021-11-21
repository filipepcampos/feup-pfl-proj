{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module BigNumber where -- TODO Change this
import Data.Char(digitToInt,ord,chr)

-- ====================================== 2.1 ====================================== 
data BigNumberSign = Positive | Negative deriving (Enum, Eq, Show)
type BigNumberDigits = [Int]
data BigNumber = BigNumber BigNumberDigits  BigNumberSign deriving (Show)

removeLeadingZerosBN :: BigNumberDigits -> BigNumberDigits -- TODO: I think this could be more efficient
removeLeadingZerosBN [] = []
removeLeadingZerosBN l = if result /= [] then result else [0]
    where result = reverse (dropWhile (== 0) (reverse l))

--   Utility functions
greaterBNDigits :: BigNumberDigits -> BigNumberDigits -> Bool
greaterBNDigits d1 d2 = if length x == length y then reverse x > reverse y else length x > length y -- TODO: These two could be merged 
    where x = removeLeadingZerosBN d1 -- TODO: This is a bit ugly and inefficient
          y = removeLeadingZerosBN d2

greatereqBNDigits :: BigNumberDigits -> BigNumberDigits -> Bool
greatereqBNDigits d1 d2 = if length x == length y then reverse x >= reverse y else length x > length y
    where x = removeLeadingZerosBN d1
          y = removeLeadingZerosBN d2

smallereqBNDigits :: BigNumberDigits -> BigNumberDigits -> Bool
smallereqBNDigits d1 d2 = not (greaterBNDigits d1 d2)
    where x = removeLeadingZerosBN d1
          y = removeLeadingZerosBN d2

getBNDigits :: BigNumber -> BigNumberDigits
getBNDigits (BigNumber d _) = d

dig2posBN :: BigNumberDigits -> BigNumber
dig2posBN d = BigNumber d Positive


-- ====================================== 2.2 ====================================== 
-- TODO: Throw error if chr is out of range ['0':'9']?
-- Scan a String and convert it to a BigNumber
scanner :: String -> BigNumber
scanner [] = error "invalid string"
scanner (h:str)
    | h == '-'  = BigNumber (reverse (map digitToInt str))     Negative
    | otherwise = BigNumber (reverse (map digitToInt (h:str))) Positive


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

operationWithCarry :: (Int -> Int -> Int -> (Int, Int)) -> BigNumberDigits -> BigNumberDigits -> Int -> BigNumberDigits
operationWithCarry func [] [] c = [c | c /= 0]
operationWithCarry func (d1:ds1) [] carry
    | carry == 0 = d1:ds1
    | otherwise = digit : operationWithCarry func [] ds1 new_carry
    where (digit,new_carry) = func d1 0 carry

operationWithCarry func [] (d2:ds2) carry
    | carry == 0 = d2:ds2
    | otherwise =  digit : operationWithCarry func [] ds2 new_carry
    where (digit,new_carry)  = func 0 d2 carry

operationWithCarry func (d1:ds1) (d2:ds2) carry = digit : operationWithCarry func ds1 ds2 new_carry
    where (digit,new_carry) = func d1 d2 carry

-- ====================================== 2.4 ====================================== 
somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN (BigNumber a Positive) (BigNumber b Negative) = subBN (BigNumber a Positive) (BigNumber b Positive)
somaBN (BigNumber a Negative) (BigNumber b Positive) = subBN (BigNumber b Positive) (BigNumber a Positive)
somaBN (BigNumber d1 s1) (BigNumber d2 s2) = BigNumber (operationWithCarry somaOp d1 d2 0) s1 -- Both numbers have same sign due to pattern matching

somaOp :: Int -> Int -> Int -> (Int, Int)
somaOp x y carry = ((x+y+carry) `mod` 10, (x+y+carry) `div` 10)


-- ====================================== 2.5 ====================================== 
subBN :: BigNumber -> BigNumber -> BigNumber
subBN (BigNumber d1 Negative) (BigNumber d2 Negative) = subBN (BigNumber d2 Positive) (BigNumber d1 Positive)
subBN (BigNumber d1 Positive) (BigNumber d2 Negative) = somaBN (BigNumber d2 Positive) (BigNumber d1 Positive)
subBN (BigNumber d1 Negative) (BigNumber d2 Positive) = somaBN (BigNumber d2 Negative) (BigNumber d1 Negative)
subBN (BigNumber d1 Positive) (BigNumber d2 Positive)
    | greatereqBNDigits d1 d2 = BigNumber (removeLeadingZerosBN(operationWithCarry subOp d1 d2 0)) Positive
    | otherwise = BigNumber (removeLeadingZerosBN(operationWithCarry subOp d2 d1 0)) Negative

subOp :: Int -> Int -> Int -> (Int, Int)
subOp x y carry = ((x-y-carry) `mod` 10, if y+carry > x then 1 else 0)

-- ====================================== 2.6 ====================================== 
mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN (BigNumber [0] _) (BigNumber _ _) = BigNumber [0] Positive -- TODO: Say this is for efficiency reasons, in reality is to avoid 0*(-1) = -0. Or find a cleaner alternative :shrug:
mulBN (BigNumber _ _) (BigNumber [0] _) = BigNumber [0] Positive
mulBN (BigNumber d1 s1) (BigNumber d2 s2) = foldr1 somaBN [BigNumber (replicate i 0 ++ mulBNDigitsbyInt d1 x) sign | (x,i) <- zip d2 [0..]]
    where sign = if s1 /= s2 then Negative else Positive

mulBNDigitsbyInt :: BigNumberDigits -> Int -> BigNumberDigits  -- TODO: This is assuming unsigned Int!
mulBNDigitsbyInt d n = operationWithCarry mulOp d (replicate (length d) n) 0

mulOp :: Int -> Int -> Int -> (Int, Int)
mulOp x y carry = ((x*y+carry) `mod` 10, (x*y+carry) `div` 10)

-- ====================================== 2.7 ====================================== 
divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN (BigNumber [0] Positive) (BigNumber _ Positive) = (BigNumber [0] Positive, BigNumber [0] Positive)
divBN (BigNumber ds1 Positive) (BigNumber [1] Positive) = (BigNumber ds1 Positive, BigNumber [0] Positive)
divBN (BigNumber ds1 Positive) (BigNumber d2 Positive) = divBNAux (init ds1) [last ds1] d2 []

smallDivide :: [Int] -> [Int] -> (BigNumberDigits, Int)
smallDivide dividend divisor = (subbed_number, m)
      where
        subbed_number = getBNDigits (subBN (dig2posBN dividend) (dig2posBN biggestMultiple))
        (biggestMultiple, m) = last (takeWhile (\(x, y) -> smallereqBNDigits x dividend) [(getBNDigits (mulBN (dig2posBN divisor) (dig2posBN [i])), i) | i <- [1 .. 9]])

divBNAux :: BigNumberDigits -> BigNumberDigits -> BigNumberDigits -> BigNumberDigits -> (BigNumber, BigNumber)
divBNAux [] dividend_r divisor quocient
    | greaterBNDigits divisor dividend_r = (dig2posBN (removeLeadingZerosBN (0:quocient)), dig2posBN (removeLeadingZerosBN dividend_r))
    | otherwise = (dig2posBN (removeLeadingZerosBN (m:quocient)), dig2posBN (removeLeadingZerosBN subNumber))
    where (subNumber, m) = smallDivide dividend_r divisor

divBNAux dividend_l dividend_r divisor quocient
    | greaterBNDigits divisor dividend_r = divBNAux (init dividend_l) (last dividend_l:dividend_r) divisor (0:quocient)
    | otherwise = divBNAux (init dividend_l) (last dividend_l : subNumber) divisor (m:quocient)
    where (subNumber, m) = smallDivide dividend_r divisor
