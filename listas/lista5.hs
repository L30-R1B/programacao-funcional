module PFexTreino05 where

-- 1. Implemente uma função que equivale à função foldl1
-- Exemplo de entrada: meuFoldl1 (+) [1,2,3,4]
meuFoldl1 :: (a -> a -> a) -> [a] -> a
meuFoldl1 _ []     = error "Lista vazia"
meuFoldl1 _ [x]    = x
meuFoldl1 f (x:xs) = foldl f x xs

-- 2. Implemente uma função que equivale à função foldr1
-- Exemplo de entrada: meuFoldr1 (+) [1,2,3,4]
meuFoldr1 :: (a -> a -> a) -> [a] -> a
meuFoldr1 _ []     = error "Lista vazia"
meuFoldr1 _ [x]    = x
meuFoldr1 f (x:xs) = f x (meuFoldr1 f xs)

-- 3. Implemente uma função que recebe uma função a->Bool e uma lista e retorna True se algum
-- elemento dessa lista obtém True com a função de entrada, ou False, caso todos os elementos
-- obtenham False.
-- Exemplo de entrada: algum even [1,3,5,7,9]
algum :: (a -> Bool) -> [a] -> Bool
algum _ [] = False
algum p (x:xs) = p x || algum p xs

-- 4. Implemente uma função que recebe uma função a->Bool e uma lista e retorna o primeiro
-- elemento dessa lista que obtém True com a função recebida. Se nenhum elemento obter True
-- com a função de entrada, o programa deve acusar erro.
-- Exemplo de entrada: primeiro even [1,3,4,6,8]
primeiro :: (a -> Bool) -> [a] -> a
primeiro _ [] = error "Nenhum elemento satisfaz a condição"
primeiro p (x:xs)
    | p x       = x
    | otherwise = primeiro p xs

-- 5. Implemente uma função que recebe um valor e uma lista de funções unárias e aplica cada
-- função sobre o resultado da anterior, começando do valor inicial. No final é retornado o valor
-- obtido pela última função da lista.
-- Exemplo de entrada: aplicaCadeia 2 [(+1), (*3), (`div` 2)]
aplicaCadeia :: a -> [a -> a] -> a
aplicaCadeia x [] = x
aplicaCadeia x (f:fs) = aplicaCadeia (f x) fs

-- 6. Implemente uma função que recebe uma lista de funções unárias e uma lista de valores e
-- retorna uma lista em que cada elemento é o resultado da aplicação da função da primeira lista
-- com o elemento da segunda que se encontra na mesma posição.
-- Exemplo de entrada: aplicaPares [(+1), (*2), (`div` 2)] [1,2,3]
aplicaPares :: [a -> b] -> [a] -> [b]
aplicaPares [] _ = []
aplicaPares _ [] = []
aplicaPares (f:fs) (x:xs) = f x : aplicaPares fs xs

-- 7. Implemente uma função que recebe uma função Int->Bool, outra função Int->Int e uma lista de
-- inteiros e retorna uma lista cujos elementos são o resultado da segunda função com os
-- elementos da lista que obtém True na primeira função.
-- Exemplo de entrada: filtraEMapeia even (*2) [1,2,3,4,5]
filtraEMapeia :: (Int -> Bool) -> (Int -> Int) -> [Int] -> [Int]
filtraEMapeia p f xs = map f (filter p xs)

-- 8. Implemente uma função que obtém a cabeça de uma lista usando a composição das funções last
-- e reverse.
-- Exemplo de entrada: cabeca [1,2,3,4]
cabeca :: [a] -> a
cabeca = last . reverse