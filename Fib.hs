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
-- ISTO ESTÁ ERRADO!!
--fibLista :: (Integral a) => Int -> a
--fibLista n = fibListaAux n !! n

-- Auxiliary Function that obtains a list with the first n fibonacci sequence numbers
--fibListaAux :: (Integral a) => Int -> [a]
--fibListaAux 0 = [0]
--fibListaAux 1 = [0, 1]
--fibListaAux n = l ++ [(l !! (n-1)) + (l !! (n-2))]
--    where l = fibListaAux (n-1)


---- 1.3 Infinite List Fibonacci Function
fibListaInfinita :: (Integral a) => Int -> a
fibListaInfinita n = dp !! n
    where dp = 0 : 1 : zipWith (+) dp (tail dp)


---- Big Numbers

---- 3.1 Recursive Big Numbers Fibonacci Function
fibRecBN :: Int -> BigNumber
-- Base Cases
fibRecBN 0 = BigNumber [0] Positive
fibRecBN 1 = BigNumber [1] Positive

-- Recursive Call
fibRecBN n  -- Fb(n) = Fb(n-1) + Fb(n-2)
    | n > 1 = somaBN (fibRecBN (n-1)) (fibRecBN (n-2))
    | otherwise = error "BigNumber must be positive"
-- TODO: reverse numbers

---- 3.3 Infinite Big Numbers List Fibonacci Function
fibListaInfinitaBN :: Int -> BigNumber
fibListaInfinitaBN n = dp !! n
    where dp = BigNumber [0] Positive : BigNumber [1] Positive : zipWith somaBN dp (tail dp)