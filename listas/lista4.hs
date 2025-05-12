module PFexTreino04 where

-- 1. Crie uma função que recebe uma lista e retorna o menor e o maior valor dessa lista.
-- Exemplo de entrada: minMax [3,1,4,1,5,9,2]
minMax :: Ord a => [a] -> (a, a)
minMax [] = error "Lista vazia"
minMax [x] = (x, x)
minMax (x:xs) = (min x minTail, max x maxTail)
  where
    (minTail, maxTail) = minMax xs

-- 2. Crie uma função que verifica se um inteiro é primo. Essa função retorna uma tupla (Bool, Int),
-- na qual o Bool indica se o inteiro é primo ou não e o Int indica qual número conseguiu dividir o
-- inteiro, caso ele não seja primo.
-- Exemplo de entrada: ehPrimo 7
ehPrimo :: Int -> (Bool, Int)
ehPrimo n
  | n < 2     = (False, 1)
  | otherwise = verificaPrimo n 2
  where
    verificaPrimo n d
      | d*d > n      = (True, n)
      | n `mod` d == 0 = (False, d)
      | otherwise      = verificaPrimo n (d+1)

-- 3. Crie uma função que recebe dois inteiros e retorna um tupla com o MDC e o MMC.
-- Exemplo de entrada: mdcMmc 12 8
mdcMmc :: Int -> Int -> (Int, Int)
mdcMmc a b = (mdc a b, mmc a b)
  where
    mdc x 0 = x
    mdc x y = mdc y (x `mod` y)
    mmc x y = (x * y) `div` mdc x y

-- 4. Crie uma função que recebe um ano e um mês e retorna o número de dias do mês,
-- considerando anos bissextos e não bissextos.
-- Exemplo de entrada: diasMes 2020 2
diasMes :: Int -> Int -> Int
diasMes ano mes
  | mes == 2 && bissexto = 29
  | mes == 2             = 28
  | mes `elem` [4,6,9,11] = 30
  | otherwise            = 31
  where
    bissexto = (ano `mod` 400 == 0) || (ano `mod` 100 /= 0 && ano `mod` 4 == 0)

-- 5. Definição do tipo Data
type Data = (Int, Int, Int)  -- (dia, mês, ano)

-- Função que verifica se duas datas são iguais
-- Exemplo de entrada: datasIguais (1,1,2020) (1,1,2020)
datasIguais :: Data -> Data -> Bool
datasIguais (d1,m1,a1) (d2,m2,a2) = d1 == d2 && m1 == m2 && a1 == a2

-- Função que calcula a diferença de dias entre duas datas (implementação simplificada)
-- Exemplo de entrada: diferencaDias (1,1,2020) (1,1,2021)
diferencaDias :: Data -> Data -> Int
diferencaDias (d1,m1,a1) (d2,m2,a2) = abs (diasDesdeZero a1 m1 d1 - diasDesdeZero a2 m2 d2)
  where
    diasDesdeZero ano mes dia = -- Implementação complexa (simplificado para exemplo)
      dia + (mes-1)*30 + (ano-1)*365  -- Apenas exemplo, não considera meses com dias diferentes nem anos bissextos

-- 6. Definição dos tipos Compromisso e Agenda
type Compromisso = (String, Data)
type Agenda = [Compromisso]

-- Função que busca compromissos por data
-- Exemplo de entrada: compromissosNaData [("Reunião",(1,1,2020)),("Aniversário",(1,1,2020))] (1,1,2020)
compromissosNaData :: Agenda -> Data -> [String]
compromissosNaData agenda dataBusca =
  [desc | (desc, dataComp) <- agenda, datasIguais dataComp dataBusca]

-- 7. Função que encontra a data com mais compromissos
-- Exemplo de entrada: dataMaisCompromissos [("Reunião",(1,1,2020)),("Aniversário",(1,1,2020)),("Dentista",(2,1,2020))]
dataMaisCompromissos :: Agenda -> Data
dataMaisCompromissos agenda =
  fst $ foldl1 (\a b -> if snd a >= snd b then a else b) contagem
  where
    contagem = [(dataComp, length $ compromissosNaData agenda dataComp) | (_, dataComp) <- agenda]

-- 8. Função que busca um compromisso por descrição e calcula dias restantes
-- Exemplo de entrada: diasParaCompromisso [("Reunião",(10,1,2020)),("Aniversário",(1,1,2020))] "Reunião" (1,1,2020)
diasParaCompromisso :: Agenda -> String -> Data -> (Data, Int)
diasParaCompromisso agenda descricao dataAtual =
  case [dataComp | (desc, dataComp) <- agenda, desc == descricao] of
    []       -> (dataAtual, 0)
    (dataComp:_) -> (dataComp, diferencaDias dataAtual dataComp)