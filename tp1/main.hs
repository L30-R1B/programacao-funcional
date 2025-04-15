module TrabalhoPratico1 where

import Data.List (intercalate)

-- Question 01
type Nome = String
type Valor = Float
type Quantidade = Int
data Produto = Produto Nome Valor deriving (Show, Eq)
data Item = Item Produto Quantidade deriving (Show)

produtos :: [Produto]
produtos = [
    Produto "AGUA MINERAL" 2.50,
    Produto "CHOCOLATE" 3.99,
    Produto "LEITE" 2.99,
    Produto "AMENDOIM" 3.99,
    Produto "HEINEKEN 5L" 64.99,
    Produto "ARROZ 5KG" 22.50,
    Produto "FEIJAO 1KG" 8.99,
    Produto "ACUCAR 1KG" 4.50,
    Produto "CAFE 500G" 12.99,
    Produto "SAL 1KG" 2.25
    ]

-- Question 02
repete :: a -> Int -> [a]
repete _ 0 = []
repete x n = x : repete x (n-1)

index :: Eq a => a -> [a] -> Maybe Int
index x xs = index' x xs 0
    where
        index' _ [] _ = Nothing
        index' x (y:ys) n
            | x == y = Just n
            | otherwise = index' x ys (n+1)

elemento :: [a] -> Int -> Maybe a
elemento [] _ = Nothing
elemento (x:_) 0 = Just x
elemento (_:xs) n
    | n < 0 = Nothing
    | otherwise = elemento xs (n-1)

-- Question 03
addProduto :: [Produto] -> Produto -> [Produto]
addProduto xs p = xs ++ [p]

remProduto :: [Produto] -> Nome -> [Produto]
remProduto [] _ = []
remProduto ((Produto nome valor):xs) nomeRem
    | nome == nomeRem = xs
    | otherwise = (Produto nome valor) : remProduto xs nomeRem

buscaProduto :: [Produto] -> Nome -> Maybe Produto
buscaProduto [] _ = Nothing
buscaProduto ((Produto nome valor):xs) nomeBusca
    | nome == nomeBusca = Just (Produto nome valor)
    | otherwise = buscaProduto xs nomeBusca

-- Question 04
alinhaEsq :: String -> Char -> Int -> String
alinhaEsq s c n
    | length s >= n = s
    | otherwise = s ++ repeteChar c (n - length s)
    where repeteChar ch m = take m (repeat ch)

alinhaDir :: String -> Char -> Int -> String
alinhaDir s c n
    | length s >= n = s
    | otherwise = repeteChar c (n - length s) ++ s
    where repeteChar ch m = take m (repeat ch)

-- Question 05
infixl 5 $$
($$) :: Valor -> Int -> String
v $$ n = 
    let s = show v
        parts = splitAtDot s
    in case parts of
        (intPart, "") -> intPart ++ ".00"
        (intPart, decPart) -> intPart ++ "." ++ take n (decPart ++ repeat '0')
    where
        splitAtDot str = case break (== '.') str of
            (a, '.':b) -> (a, b)
            (a, _) -> (a, "")

dinheiro :: Valor -> String
dinheiro v = "$" ++ (v $$ 2)

-- Question 06
formataItem :: Item -> String
formataItem (Item (Produto nome valor) qtd) =
    let nomeFormatado = alinhaEsq nome '.' 45
        valorStr = dinheiro valor
        qtdStr = show qtd
        subtotal = dinheiro (valor * fromIntegral qtd)
        linha = nomeFormatado ++ " " ++ valorStr ++ " x " ++ qtdStr ++ " = " ++ subtotal
    in take 80 linha

-- Question 07
total :: [Item] -> String
total itens = dinheiro (somaTotal itens)
    where
        somaTotal [] = 0
        somaTotal ((Item (Produto _ valor) qtd):xs) = 
            valor * fromIntegral qtd + somaTotal xs

-- Question 08
notafiscal :: [Item] -> String
notafiscal itens =
    let header = repeteChar '*' 80
        title = "NOTA FISCAL"
        titleLine = alinhaEsq (alinhaDir title ' ' ((80 - length title) `div` 2 + length title)) ' ' 80
        itemsLines = map formataItem itens
        totalLine = "TOTAL: " ++ total itens
        footer = repeteChar '*' 80
    in intercalate "\n" (header : titleLine : itemsLines ++ [footer, totalLine, footer])
    where repeteChar ch n = take n (repeat ch)

-- Question 09
proditem :: [Item]
proditem = proditemx [1..length produtos]

proditemx :: [Quantidade] -> [Item]
proditemx qtds = zipWith (\p q -> Item p q) (take (length qtds) produtos) qtds

-- Question 10
itensn :: [(Nome, Quantidade)] -> [Item]
itensn = map (\(nome, qtd) -> case buscaProduto produtos nome of
    Just p -> Item p qtd
    Nothing -> error "Produto não encontrado")

itensi :: [(Int, Quantidade)] -> [Item]
itensi = map (\(idx, qtd) -> case elemento produtos idx of
    Just p -> Item p qtd
    Nothing -> error "Índice de produto inválido")

-- Question 11
venda :: [Item] -> IO()
venda itens = putStrLn (notafiscal itens)