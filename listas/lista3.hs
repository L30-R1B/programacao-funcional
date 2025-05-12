module PFexTreino03 where

-- 1. Crie uma função que inverte uma lista usando recursão em cauda.
-- Exemplo de entrada: inverteListaTail [1, 2, 3]
inverteListaTail :: [a] -> [a]
inverteListaTail xs = inverteAux xs []
  where
    inverteAux [] acc     = acc
    inverteAux (x:xs) acc = inverteAux xs (x:acc)

-- 2. Crie uma função que recebe uma lista de notas (Float) de alunos, calcula a média e retorna
-- uma String como resultado, sendo que: se a média < 4.0, o resultado é "REPROVADO"; se a
-- 4.0 <= média < 6.0, o resultado é "RECUPERACAO"; e se média >= 6.0, o resultado é
-- "APROVADO".
-- Exemplo de entrada: resultadoFinal [5.0, 6.0, 4.0]
resultadoFinal :: [Float] -> String
resultadoFinal notas
  | media < 4.0  = "REPROVADO"
  | media < 6.0  = "RECUPERACAO"
  | otherwise    = "APROVADO"
  where
    media = sum notas / fromIntegral (length notas)

-- 3. Crie uma função que recebe um inteiro n e retorna uma lista com os n primeiros elementos da
-- série de Fibonacci.
-- Exemplo de entrada: fibonacci 5
fibonacci :: Int -> [Integer]
fibonacci n = take n fibSeq
  where
    fibSeq = 0 : 1 : zipWith (+) fibSeq (tail fibSeq)

-- 4. Crie a função equacao2grau a b c que retorna as raízes reais da equação do segundo grau
-- (ax² + bx + c = 0) em uma lista (a não pode ser zero). Use definição local para calcular o delta.
-- Exemplo de entrada: equacao2grau 1 (-5) 6
equacao2grau :: Float -> Float -> Float -> [Float]
equacao2grau a b c
  | a == 0    = error "O coeficiente 'a' não pode ser zero"
  | delta < 0 = []
  | otherwise = [(-b - sqrt delta) / (2*a), (-b + sqrt delta) / (2*a)]
  where
    delta = b^2 - 4*a*c

-- 5. Crie uma função que recebe um inteiro n e uma lista e retorna uma lista de listas que
-- corresponde à lista original dividida em n partes.
-- Exemplo de entrada: divideEmPartes 3 [1..9]
divideEmPartes :: Int -> [a] -> [[a]]
divideEmPartes n xs
  | n <= 0    = error "Número de partes deve ser positivo"
  | otherwise = divideAux n (length xs) xs
  where
    divideAux _ _ [] = []
    divideAux n len xs = take partSize xs : divideAux n len (drop partSize xs)
      where
        partSize = ceiling (fromIntegral len / fromIntegral n)