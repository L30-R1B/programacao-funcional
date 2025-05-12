module TrabalhoPratico where

import Data.List (intercalate)

-- Questão 01: Definição de tipos
type Nome = String
type Valor = Float
type Quantidade = Int

data Produto = Produto { nomeProduto :: Nome, valorProduto :: Valor } deriving (Show, Eq)
data Item = Item { produtoItem :: Produto, quantidadeItem :: Quantidade } deriving (Show)

-- Lista de produtos
produtos :: [Produto]
produtos = [
    Produto "AGUA MINERAL" 2.50,
    Produto "CHOCOLATE" 3.99,
    Produto "LEITE" 2.99,
    Produto "CAFE" 5.99,
    Produto "ARROZ" 10.50,
    Produto "FEIJAO" 8.75,
    Produto "ACUCAR" 4.25,
    Produto "SAL" 1.99,
    Produto "OLEO" 7.50,
    Produto "BISCOITO" 3.25
    ]

-- Questão 02: Funções auxiliares
repete :: a -> Int -> [a]
repete _ 0 = []
repete x n = x : repete x (n-1)

index :: Eq a => a -> [a] -> Maybe Int
index x xs = index' x xs 0
    where
        index' _ [] _ = Nothing
        index' y (z:zs) i
            | y == z = Just i
            | otherwise = index' y zs (i+1)

elemento :: [a] -> Int -> Maybe a
elemento [] _ = Nothing
elemento (x:_) 0 = Just x
elemento (_:xs) n
    | n < 0 = Nothing
    | otherwise = elemento xs (n-1)

-- Questão 03: Operações com produtos
addProduto :: [Produto] -> Produto -> [Produto]
addProduto ps p = ps ++ [p]

remProduto :: [Produto] -> Nome -> [Produto]
remProduto [] _ = []
remProduto (p:ps) nome
    | nomeProduto p == nome = ps
    | otherwise = p : remProduto ps nome

buscaProduto :: [Produto] -> Nome -> Maybe Produto
buscaProduto [] _ = Nothing
buscaProduto (p:ps) nome
    | nomeProduto p == nome = Just p
    | otherwise = buscaProduto ps nome

-- Questão 04: Alinhamento de strings
alinhaEsq :: String -> Char -> Int -> String
alinhaEsq str ch n = str ++ replicate (n - length str) ch

alinhaDir :: String -> Char -> Int -> String
alinhaDir str ch n = replicate (n - length str) ch ++ str

-- Questão 05: Formatação de valores monetários
infix 5 $$
($$) :: Valor -> Int -> String
val $$ dec =
    let inteiro = show (truncate val :: Int)
        parteDecimal = val - fromIntegral (truncate val :: Int)
        decimalStr = show (round (parteDecimal * 10^dec) :: Int)
        decimalPadded = replicate (dec - length decimalStr) '0' ++ decimalStr
    in inteiro ++ "." ++ take dec decimalPadded

dinheiro :: Valor -> String
dinheiro val = "$" ++ (val $$ 2)

-- Questão 06: Formatação de itens
formataItem :: Item -> String
formataItem (Item (Produto nome valor) qtd) =
    let nomeFormatado = alinhaEsq nome '.' 45
        valorFormatado = alinhaDir (dinheiro valor) ' ' 10
        qtdFormatado = show qtd
        subtotal = alinhaDir (dinheiro (valor * fromIntegral qtd)) ' ' 10
        linha = nomeFormatado ++ " " ++ valorFormatado ++ " x " ++ qtdFormatado ++ " = " ++ subtotal
    in take 80 linha

-- Questão 07: Cálculo do total
total :: [Item] -> String
total itens = dinheiro (sum [valorProduto (produtoItem i) * fromIntegral (quantidadeItem i) | i <- itens])

-- Questão 08: Nota fiscal
notaFiscal :: [Item] -> String
notaFiscal itens =
    let linhaSeparadora = replicate 80 '*'
        cabecalho = "NOTA FISCAL"
        linhasItens = map formataItem itens
        totalStr = "TOTAL: " ++ total itens
    in unlines $ [linhaSeparadora, cabecalho, linhaSeparadora] ++ linhasItens ++ [linhaSeparadora, totalStr, linhaSeparadora]

-- Questão 09: Geração de itens
prodItem :: [Item]
prodItem = [Item p (i+1) | (p, i) <- zip produtos [0..]]

prodItemX :: [Quantidade] -> [Item]
prodItemX qtds = take (length qtds) [Item p q | (p, q) <- zip produtos qtds]

-- Questão 10: Conversão de listas para itens
itensN :: [(Nome, Quantidade)] -> [Item]
itensN nqs = [Item p q | (n, q) <- nqs, Just p <- [buscaProduto produtos n]]

itensI :: [(Int, Quantidade)] -> [Item]
itensI iqs = [Item p q | (i, q) <- iqs, Just p <- [elemento produtos i]]

-- Questão 11: Função de venda
venda :: [Item] -> IO ()
venda itens = putStrLn (notaFiscal itens)