import BigNumber

---- 1.1 Recursive Fibonacci Function
fibRec :: (Integral a) => a -> a
-- Base Cases
fibRec 0 = 0
fibRec 1 = 1

-- Recursive Call
fibRec n  -- Fb(n) = Fb(n-1) + Fb(n-2)
    | n > 1 = fibRec (n-1) + fibRec (n-2)
    | otherwise = error "n must be positive"


---- 1.2 Dynamic Programming Fibonacci Function
fibLista :: (Integral a) => Int -> a
fibLista n = fibListaAux n !! n

-- Auxiliary Function that obtains a list with the first n fibonacci sequence numbers
fibListaAux :: (Integral a) => Int -> [a]
fibListaAux 0 = [0]
fibListaAux 1 = [0, 1]
fibListaAux n = l ++ [(l !! (n-1)) + (l !! (n-2))]  -- Append the sum of the last two elements to the list
    where l = fibListaAux (n-1)


---- 1.3 Infinite List Fibonacci Function
fibListaInfinita :: (Integral a) => Int -> a
fibListaInfinita n = dp !! n
    where dp = 0 : 1 : zipWith (+) dp (tail dp)  -- Zip a list with with the same list without the first element and 
                                                 -- calculate the sum of the elements with the same index infinitely

---- Big Numbers -- TODO: reverse numbers and 3.2 / 3.3


---- 3.1 Recursive Big Numbers Fibonacci Function
fibRecBN :: BigNumber -> BigNumber
-- Base Cases
fibRecBN (BigNumber [0] _) = BigNumber [0] Positive
fibRecBN (BigNumber [1] Positive) = BigNumber [1] Positive

-- Recursive Call
fibRecBN (BigNumber n signal)  -- Fb(n) = Fb(n-1) + Fb(n-2)
    | signal == Positive = somaBN (fibRecBN previous) (fibRecBN (subBN previous (BigNumber [1] Positive)))
    | otherwise = error "BigNumber must be positive"
    where previous = subBN (BigNumber n signal) (BigNumber [1] Positive)


---- 1.2 Dynamic Programming Fibonacci Function

fibListaBN :: BigNumber -> BigNumber
fibListaBN (BigNumber n signal)
    | signal == Positive = fibListaAux n !! n
    | otherwise = error "BigNumber must be positive"

-- Auxiliary Function that obtains a list with the first n fibonacci sequence numbers
fibListaBNAux :: BigNumber -> [BigNumber]
fibListaBNAux (BigNumber [0] _) = [BigNumber [0] Positive]
fibListaBNAux (BigNumber [1] Positive) = [BigNumber [0] Positive, BigNumber [1] Positive]
fibListaBNAux (BigNumber n signal) = l ++ [(l !! (n-1)) + (l !! (n-2))]
    where l = fibListaBNAux previous
previous = subBN (BigNumber n signal) (BigNumber [1] Positive)


---- 3.3 Infinite Big Numbers List Fibonacci Function
fibListaInfinitaBN :: Int -> BigNumber
fibListaInfinitaBN n
    | n >= 0 = dp !! n
    | otherwise = error "BigNumber must be positive"
    where dp = BigNumber [0] Positive : BigNumber [1] Positive : zipWith somaBN dp (tail dp)