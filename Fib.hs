import BigNumber

-- ====================================== 1.1 ====================================== 
---- Recursive Fibonacci Function
fibRec :: (Integral a) => a -> a
-- Base Cases
fibRec 0 = 0
fibRec 1 = 1

-- Recursive Call
fibRec n  -- Fb(n) = Fb(n-1) + Fb(n-2)
    | n > 1 = fibRec (n-1) + fibRec (n-2)
    | otherwise = error "n must be positive"

-- ====================================== 1.2 ====================================== 
---- Dynamic Programming Fibonacci Function
fibLista :: (Integral a) => (Integral a) => a -> a
fibLista n = fibListaAux (fromIntegral n) !! fromIntegral n

-- Auxiliary Function that obtains a list with the first n fibonacci sequence numbers
fibListaAux :: (Integral a) => Int -> [a]
fibListaAux 0 = [0]
fibListaAux 1 = [0, 1]
fibListaAux n = l ++ [(l !! (n-1)) + (l !! (n-2))]  -- Append the sum of the last two elements to the list
    where l = fibListaAux (n-1)

-- ====================================== 1.3 ====================================== 
---- Infinite List Fibonacci Function
fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n = dp !! fromIntegral n
    where dp = 0 : 1 : zipWith (+) dp (tail dp)  -- Zip a list with with the same list without the first element and 
                                                 -- calculate the sum of the elements with the same index infinitely


-- ====================================== 3.1 ====================================== 
---- Recursive Big Numbers Fibonacci Function
fibRecBN :: BigNumber -> BigNumber
-- Base Cases
fibRecBN (BigNumber [0] _) = BigNumber [0] Positive
fibRecBN (BigNumber [1] Positive) = BigNumber [1] Positive

-- Recursive Call
fibRecBN (BigNumber n signal)  -- Fb(n) = Fb(n-1) + Fb(n-2)
    | signal == Positive = somaBN (fibRecBN previous) (fibRecBN (decBN previous))
    | otherwise = error "BigNumber must be positive"
    where previous = decBN (BigNumber n signal)

-- ====================================== 3.2 ====================================== 
---- Dynamic Programming Big Numbers Fibonacci Function
fibListaBN :: BigNumber -> BigNumber
fibListaBN (BigNumber n signal)
    | signal == Positive = fibListaBNAux (BigNumber n signal) `indexBN` (BigNumber n signal)
    | otherwise = error "BigNumber must be positive"

-- Auxiliary Function that obtains a list with the first n fibonacci sequence numbers
fibListaBNAux :: BigNumber -> [BigNumber]
fibListaBNAux (BigNumber [0] _) = [BigNumber [0] Positive]
fibListaBNAux (BigNumber [1] Positive) = [BigNumber [0] Positive, BigNumber [1] Positive]
fibListaBNAux (BigNumber n signal) = l ++ [(l `indexBN` previous) `somaBN` (l `indexBN` (decBN previous))]
    where l = fibListaBNAux previous
          previous = decBN (BigNumber n signal)

-- ====================================== 3.3 ====================================== 
---- Infinite Big Numbers List Fibonacci Function
fibListaInfinitaBN :: BigNumber  -> BigNumber
fibListaInfinitaBN (BigNumber n signal)
    | signal == Positive = dp `indexBN` (BigNumber n signal)
    | otherwise = error "BigNumber must be positive"
    where dp = BigNumber [0] Positive : BigNumber [1] Positive : zipWith somaBN dp (tail dp)