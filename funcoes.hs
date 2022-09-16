------------------------------------------------------------------------------------------------------
--  Disciplina: Programação Funcional
--  Integrantes: Alison Schemitt, Cassiano Flores, Leonardo Cruz, Mateus de Carvalho
------------------------------------------------------------------------------------------------------


--Funcao 1 - OK
bin2dec :: [Int] -> Int
bin2dec [] = 0
bin2dec (s:xs) = (s * 2 ^ (length xs)) + bin2dec xs

--Funcao 2 - 0 em binário deveria ser 0 e não []
dec2bin :: Int -> [Int]
dec2bin 0 = []
dec2bin n | n `mod` 2 == 1 = dec2bin (n `div` 2) ++ [1]
          | n `mod` 2 == 0 = dec2bin (n `div` 2) ++ [0]

--Funcao 3 - OK
bincompl2dec :: [Int] -> Int
bincompl2dec (s:xs) = (-1 * (s * 2 ^ (length xs))) + bin2dec xs

--Funcao 4 - OK, mas acrescenta 1 à esquerda
dec2bincompl :: Int -> [Int]
dec2bincompl x = if x == 0 then [0] else
    if x > 0 then ([0] ++ (dec2bin x)) else dec2bin (bin2dec (not_op ([0] ++ (dec2bin ((-1) * x)))) + 1) 

not_op :: [Int] -> [Int]
not_op [] = []
not_op (x:xs) = 
    if x == 0   
        then [1] ++ (not_op xs)
        else [0] ++ (not_op xs)

--Funcao 5 - OK, mas coloca bits a mais à esquerda (ex.: 01 + 01 = 010 ao invés de 10 indicando um overflow)
somarbin :: [Int] -> [Int] -> [Int]
somarbin a b = dec2bincompl ((bincompl2dec a) + (bincompl2dec b))

--Funcao 6 - OK, idem à soma
subtrairbin :: [Int] -> [Int] -> [Int]
subtrairbin a b = dec2bincompl ((bincompl2dec a) - (bincompl2dec b))

--Funcao 7 - OK
andbin :: [Int] -> [Int] -> [Int]
andbin [] [] = [] ++ []
andbin (a:ax) (b:bx) = (if(a + b) == 2 then [1] else [0]) ++ andbin ax bx

--Funcao 8 - OK
orbin :: [Int] -> [Int] -> [Int]
orbin [] [] = [] ++ []
orbin (a:ax) (b:bx) = (if(a + b) > 0 then [1] else [0]) ++ orbin ax bx

--Funcao 9 - implementação incorreta do conceito de número binário fracionário na parte da fração
            -- (conversão não faz sentido, por exemplo, 0.5 é 0.1 em binário e não 0.1010)
frac2bin :: Double -> ([Int], [Int])
frac2bin x = (   dec2bincompl(floor x) , dec2bin(floor(primeiro_decimal x)) ++ [0] )

primeiro_decimal :: Double -> Double
primeiro_decimal x = snd (properFraction x) * 10

--Funcao 10 - implementação incorreta do conceito de número binário fracionário na parte da fração
            -- (a conversão não é direta de um dígito binário para decimal, pois são potências negativas)
bin2frac :: ([Int], [Int]) -> Double
bin2frac (a,b) =  exibe_double (bincompl2dec a) (bin2dec b)

exibe_double :: Int -> Int -> Double
exibe_double a b = read ((show a) ++ "." ++ (show b))
