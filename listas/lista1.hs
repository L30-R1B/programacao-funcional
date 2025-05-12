module PFexTreino01 where

-- 1. Crie uma função que recebe o raio (Double) de um círculo e retorna o perímetro dele.
-- Exemplo de entrada: perimetroCirculo 5.0
perimetroCirculo :: Double -> Double
perimetroCirculo r = 2 * pi * r

-- 2. Crie uma função que recebe 3 valores Float e calcula a média entre eles.
-- Exemplo de entrada: media3 3.0 4.0 5.0
media3 :: Float -> Float -> Float -> Float
media3 a b c = (a + b + c) / 3.0

-- 3. Crie uma função que recebe um Char e indica se é um dígito ou não.
-- Exemplo de entrada: ehDigito '5'
ehDigito :: Char -> Bool
ehDigito c = c >= '0' && c <= '9'

-- 4. Crie uma função que converte uma letra minúscula (Char) para maiúscula.
-- Exemplo de entrada: paraMaiuscula 'a'
paraMaiuscula :: Char -> Char
paraMaiuscula c
    | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32) :: Char
    | otherwise = c

-- 5. Crie um operador que calcula a média entre 2 valores do tipo Double.
-- Exemplo de entrada: 4.0 </> 2.0
(</>) :: Double -> Double -> Double
a </> b = (a + b) / 2.0

-- 6. Crie uma função que calcula a soma entre dois inteiros (conjunto fechado).
-- Exemplo de entrada: somaIntervalo 1 5
somaIntervalo :: Int -> Int -> Int
somaIntervalo a b = sum [min a b .. max a b]

-- 7. Crie um operador que calcula a média dos números entre dois inteiros (conjunto fechado).
-- Exemplo de entrada: 1 <//> 5
(<//>) :: Int -> Int -> Double
a <//> b = fromIntegral (somaIntervalo a b) / fromIntegral (abs (b - a) + 1)

-- 8. Crie uma função que calcula a potência entre dois inteiros (não use o operador ^).
-- Exemplo de entrada: potencia 2 3
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exp = base * potencia base (exp - 1)

-- 9. Crie um operador que calcula o resto da divisão entre inteiros positivos (não use as funções mod ou rem).
-- Exemplo de entrada: 7 <%> 3
(<%>) :: Int -> Int -> Int
a <%> b
    | a < b = a
    | otherwise = (a - b) <%> b

-- 10. Crie uma função que calcula o mdc entre dois inteiros positivos (não use a função gcd).
-- Exemplo de entrada: mdc 12 8
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a <%> b)

-- 11. Crie uma função que calcula o mmc entre dois inteiros positivos (não use a função lcm).
-- Exemplo de entrada: mmc 12 8
mmc :: Int -> Int -> Int
mmc a b = (a * b) `div` (mdc a b)

-- 12. Crie uma função que recebe um Int e retorna quantos algarismos ele possui.
-- Exemplo de entrada: contaAlgarismos 12345
contaAlgarismos :: Int -> Int
contaAlgarismos n
    | n < 0 = contaAlgarismos (abs n)
    | n < 10 = 1
    | otherwise = 1 + contaAlgarismos (n `div` 10)

-- 13. Crie uma função que recebe dois Int, sendo o primeiro um inteiro positivo ou negativo com qualquer quantidade de algarismos e o segundo um único algarismo (positivo). A função deve retornar quantas vezes o segundo algarismo aparece no primeiro inteiro.
-- Exemplo de entrada: contaOcorrencias 12321 2
contaOcorrencias :: Int -> Int -> Int
contaOcorrencias n d
    | n < 0 = contaOcorrencias (abs n) d
    | n < 10 = if n == d then 1 else 0
    | otherwise = (if (n `mod` 10) == d then 1 else 0) + contaOcorrencias (n `div` 10) d

-- 14. Crie uma função que recebe um inteiro positivo e retorna uma String que corresponde ao inteiro convertido para binário.
-- Exemplo de entrada: paraBinario 5
paraBinario :: Int -> String
paraBinario 0 = "0"
paraBinario n
    | n < 0 = error "Número negativo não suportado"
    | otherwise = paraBinarioAux n ""
    where
        paraBinarioAux 0 str = str
        paraBinarioAux n str = paraBinarioAux (n `div` 2) (show (n `mod` 2) ++ str)

-- 15. Crie uma função que calcule a soma dos algarismos de um número inteiro. Por exemplo, se a entrada for 123, a saída deverá ser 1+2+3 = 6.
-- Exemplo de entrada: somaAlgarismos 123
somaAlgarismos :: Int -> Int
somaAlgarismos n
    | n < 0 = somaAlgarismos (abs n)
    | n < 10 = n
    | otherwise = (n `mod` 10) + somaAlgarismos (n `div` 10)

-- 16. Crie uma função que calcula um elemento da série de Ackermann, que recebe dois inteiros não negativos e é definida por:
-- A(m, n) = n + 1, se m = 0
-- A(m, n) = A(m - 1, 1), se m > 0 e n = 0
-- A(m, n) = A(m - 1, A(m, n - 1)), se m > 0 e n > 0.
-- Exemplo de entrada: ackermann 1 1
ackermann :: Int -> Int -> Int
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))