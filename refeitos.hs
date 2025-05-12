--l1e1
perimetroCirculo :: Double -> Double
perimetroCirculo r = 2 * pi * r

--l1e2
media3 :: Float -> Float -> Float -> Float
media3 a b c = (a + b + c) / 3.0

--l1e3
ehDigito :: Char -> Bool
ehDigito c = c >= '0' && c <= '9'

--l1e4
paraMaiuscula :: Char -> Char
paraMaiuscula c 
    | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32) :: Char
    | otherwise = c

--l1e5
(?:) :: Double -> Double -> Double
(?:) a b = (a + b) / 2.0

--l1e6
somaIntervalo :: Int -> Int -> Int
somaIntervalo a b = sum [min a b .. max a b]

--l1e7
(++/) :: Int -> Int -> Double
a ++/ b = fromIntegral (somaIntervalo a b) / fromIntegral (abs (b - a) + 1)

--l1e8
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exp = base * potencia base (exp - 1)

--l1e9
(%/%) :: Int -> Int -> Int
a %/% b
    | a < b = a
    | otherwise = (a - b) %/% b

--l1e10
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a %/% b) 

--l1e11
mmc :: Int -> Int -> Int
mmc 1 b = b
mmc a 1 = a
mmc a b = (a * b) `div` (mmc a b)

--l1e12
conta :: Int -> Int
conta n 
    | n < 0 = conta (abs n)
    | n < 10 = 1
    | otherwise = 1 + conta (n `div` 10)

--l1e13
ocorrencias :: Int -> Int -> Int
ocorrencias a b
    | a < 0 = ocorrencias (abs a) b
    | a < 10 = if a == b then 1 else 0
    | otherwise = (if (a `mod` 10) == b then 1 else 0) + ocorrencias (a `div` 10) b

--l1e14

--l1e15
somaAlgarismos :: Int -> Int
somaAlgarismos n
    | n < 0 = somaAlgarismos (abs n)
    | n < 10 = n
    | otherwise = (n `mod` 10) + somaAlgarismos (n `mod` 10)

--l1e16

--l2e1
mediaLista :: [Double] -> Double
mediaLista x = sum x / fromIntegral (length x)

--l2e2
inverteLista :: [a] -> [a]
inverteLista [] = []
inverteLista (x:xf) = inverteLista xf ++ [x]