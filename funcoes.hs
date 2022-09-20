------------------------------------------------------------------------------------------------------
--  Disciplina: Programação Funcional
--  Integrantes: Alison Schemitt, Cassiano Flores, Leonardo Cruz, Mateus de Carvalho
------------------------------------------------------------------------------------------------------


--Funcao 1 - OK
bin2dec :: [Int] -> Int
bin2dec [] = 0
bin2dec (s:xs) = (s * 2 ^ (length xs)) + bin2dec xs

--Funcao 2 - Ok
dec2bin :: Int -> [Int]
dec2bin 0 = [0]
dec2bin n = dec2binaux n

dec2binaux :: Int -> [Int]
dec2binaux 0 = []
dec2binaux n | n `mod` 2 == 1 = dec2binaux (n `div` 2) ++ [1]
             | n `mod` 2 == 0 = dec2binaux (n `div` 2) ++ [0]

--Funcao 3 - OK
bincompl2dec :: [Int] -> Int
bincompl2dec (s:xs) = (-1 * (s * 2 ^ (length xs))) + bin2dec xs

--Funcao 4 - OK 
dec2bincompl :: Int -> [Int]
dec2bincompl x = if x == 0 then [0] 
                else if x > 0 then ([0] ++ (dec2bin x))
                else dec2bin (bin2dec (not_op ([0] ++ (dec2bin ((-1) * x)))) + 1) 

not_op :: [Int] -> [Int]
not_op [] = []
not_op (x:xs) = 
    if x == 0   
        then [1] ++ (not_op xs)
        else [0] ++ (not_op xs)

--Funcao 5 - OK
somarbin :: [Int] -> [Int] -> [Int]
somarbin a b = dec2bincompl ((bincompl2dec a) + (bincompl2dec b))

--Funcao 6 - OK
subtrairbin :: [Int] -> [Int] -> [Int]
subtrairbin a b = dec2bincompl ((bincompl2dec a) - (bincompl2dec b))

--Funcao 7 -  valores negativos ta estranho para acertar teria que juntar as duas partes 
           -- fazer o dec2bincompl e separar denovoou deixa assim e daleekk
frac2bin :: Double -> ([Int], [Int])
frac2bin x = if x >= 0 then (dec2bincompl(floor x), if (snd( properFraction(-1 * x)) == 0.0) then [0]
                                                    else take 32 (parte_frac (snd (properFraction x))))
             else   (dec2bincompl( -1 * floor(-1*x)), if (snd( properFraction(-1 * x)) == 0.0) then [0]
                                                    else take 32 ( parte_frac( snd( properFraction(-1 * x)))))

parte_frac :: Double -> [Int]
parte_frac 0 = []
parte_frac n = ([floor(n*2)] ++ parte_frac (snd (properFraction (n*2))))


--Funcao 8 - implementação incorreta do conceito de número binário fracionário na parte da fração
            -- (a conversão não é direta de um dígito binário para decimal, pois são potências negativas)
bin2frac :: ([Int], [Int]) -> Double
bin2frac (a,b) =  exibe_double (bincompl2dec a) (bin2dec b)

exibe_double :: Int -> Int -> Double
exibe_double a b = read ((show a) ++ "." ++ (show b))
