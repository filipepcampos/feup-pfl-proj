import Test.QuickCheck
import BigNumber

prop_scanout :: Integer -> Bool
prop_scanout a = show a == BigNumber.output (scanner(show a))

-- Assuming Haskell devs know how to implement these operations...
prop_somaBN :: Integer -> Integer -> Bool
prop_somaBN a b = show (a + b) == BigNumber.output(somaBN (scanner (show a)) (scanner (show b)))

prop_subBN :: Integer -> Integer -> Bool
prop_subBN a b = show (a - b) == BigNumber.output(subBN (scanner (show a)) (scanner (show b)))

prop_mulBN :: Integer -> Integer -> Bool 
prop_mulBN a b = show (a*b) == BigNumber.output(mulBN (scanner (show a)) (scanner (show b)))

main :: IO()
main = do
    putStrLn "> scanout"
    quickCheck prop_scanout
    putStrLn "> somaBN"
    quickCheck prop_somaBN
    putStrLn "> subBN"
    quickCheck prop_subBN
    putStrLn "> mulBN"
    quickCheck prop_mulBN