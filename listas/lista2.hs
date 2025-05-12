module PFexTreino02 where
import Data.Char (toUpper)

-- 1. Implemente uma função que recebe uma lista do tipo Double e retorne a média dos elementos dessa lista.
-- Exemplo de entrada: mediaLista [1.0, 2.0, 3.0]
mediaLista :: [Double] -> Double
mediaLista xs = sum xs / fromIntegral (length xs)

-- 2. Implemente uma função que inverte os elementos de uma lista (não use a função reverse).
-- Exemplo de entrada: inverteLista [1, 2, 3]
inverteLista :: [a] -> [a]
inverteLista [] = []
inverteLista (x:xs) = inverteLista xs ++ [x]

-- 3. Implemente uma função que informe o n-ésimo membro de uma lista (não use o operador !!).
-- Exemplo de entrada: enesimo 2 [1, 2, 3, 4]
enesimo :: Int -> [a] -> a
enesimo _ [] = error "Índice fora da lista"
enesimo 0 (x:_) = x
enesimo n (_:xs) = enesimo (n - 1) xs

-- 4. Implemente uma função que recebe uma String e a retorna com todas as letras em maiúscula.
-- Exemplo de entrada: paraMaiusculas "hello"
paraMaiusculas :: String -> String
paraMaiusculas = map toUpper
    where
        toUpper c
            | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32) :: Char
            | otherwise = c

-- 5. Implemente uma função que recebe uma frase (String) e a retorna com as letras iniciais de cada palavra em maiúscula.
-- Exemplo de entrada: capitalizaFrase "hello world"
capitalizaFrase :: String -> String
capitalizaFrase [] = []
capitalizaFrase (x:xs) = toUpper x : capitalizaPalavra xs
    where
        capitalizaPalavra [] = []
        capitalizaPalavra (x:xs)
            | x == ' '  = ' ' : capitalizaFrase xs
            | otherwise = x : capitalizaPalavra xs

-- 6. Implemente uma função que recebe uma String e retorna outra somente com as letras.
-- Exemplo de entrada: filtraLetras "a1b2c3"
filtraLetras :: String -> String
filtraLetras = filter (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

-- 7. Implemente uma função que recebe uma lista e um inteiro t e retorna uma lista de listas que corresponde a lista original dividida em listas de tamanho t.
-- Exemplo de entrada: divideLista 2 [1, 2, 3, 4, 5]
divideLista :: Int -> [a] -> [[a]]
divideLista _ [] = []
divideLista t xs = take t xs : divideLista t (drop t xs)

-- 8. Implemente uma função que verifica se uma lista é ou não um palíndromo. Um palíndromo é uma lista que se invertida se mantém exatamente igual à original.
-- Exemplo de entrada: ehPalindromo "radar"
ehPalindromo :: Eq a => [a] -> Bool
ehPalindromo xs = xs == inverteLista xs

-- 9. Implemente uma função que recebe um inteiro e retorna uma lista de todos os números primos iguais ou inferiores a esse inteiro.
-- Exemplo de entrada: primosAte 10
primosAte :: Int -> [Int]
primosAte n = filter isPrime [2..n]
    where
        isPrime x = null [y | y <- [2..x-1], x `mod` y == 0]

-- 10. Implemente uma função que recebe uma lista de listas e retorna uma lista normal com todos os elementos das listas internas (não use a função concat).
-- Exemplo de entrada: concatenaListas [[1, 2], [3, 4]]
concatenaListas :: [[a]] -> [a]
concatenaListas [] = []
concatenaListas (xs:xss) = xs ++ concatenaListas xss

-- 11. Implemente uma função que recebe uma lista de listas do tipo Double e retorna uma lista com a média dos elementos de cada lista interna.
-- Exemplo de entrada: mediaListas [[1.0, 2.0], [3.0, 4.0, 5.0]]
mediaListas :: [[Double]] -> [Double]
mediaListas = map mediaLista