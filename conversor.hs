
--Funcao 1
bin2dec :: [Int] -> Int
bin2dec [] = 0
bin2dec (s:xs) = (s * 2 ^ (length xs)) + bin2dec xs

--Funcao 2
dec2bin :: Int -> [Int]
dec2bin 0 = [0]
dec2bin n = dec2binaux n

--faz a conversao e auxilia o caso com o zero para nao retornar []
dec2binaux :: Int -> [Int]
dec2binaux 0 = []
dec2binaux n | n `mod` 2 == 1 = dec2binaux (n `div` 2) ++ [1]
             | n `mod` 2 == 0 = dec2binaux (n `div` 2) ++ [0]

--Funcao 3
bincompl2dec :: [Int] -> Int
bincompl2dec (s:xs) = (-1 * (s * 2 ^ (length xs))) + bin2dec xs

--Funcao 4
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

--Funcao 5
somarbin :: [Int] -> [Int] -> [Int]
somarbin a b = dec2bincompl ((bincompl2dec a) + (bincompl2dec b))

--Funcao 6
subtrairbin :: [Int] -> [Int] -> [Int]
subtrairbin a b = dec2bincompl ((bincompl2dec a) - (bincompl2dec b))



-- **** seguimos o enunciado onde diz - 'a parte inteira com representação de complemento de dois com no maximo 32 bis'
-- **** não está claro se queria no padrão IEEE 754

--Funcao 7
frac2bin :: Double -> ([Int], [Int])
frac2bin x = if x >= 0 then ( take 32 (dec2bincompl(floor x)), take 32 (parte_frac x))
             else ( take 32 (dec2bincompl( -1 * floor(-1*x))), take 32 (parte_frac x))

--verifica o sinal
parte_frac ::Double -> [Int]
parte_frac n  | snd( properFraction n) == 0.0 = [0]
                    | snd( properFraction n) > 0 = parte_frac_aux( snd( properFraction n))
                    | snd( properFraction n) < 0 = parte_frac_aux( snd( properFraction(-1 * n)))

--faz a parte de conversao
parte_frac_aux :: Double -> [Int]
parte_frac_aux 0 = []
parte_frac_aux n = ([floor(n*2)] ++ parte_frac_aux (snd (properFraction (n*2))))


--Funcao 8
bin2frac :: ([Int], [Int]) -> Double
bin2frac (a,b)  | (bincompl2dec a) >= 0 = (fromIntegral(bincompl2dec (take 32 a))) + (bin2frac_aux (take 32 b))
                | (bincompl2dec a) < 0 = (fromIntegral(bincompl2dec (take 32 a))) - (bin2frac_aux (take 32 b))

--converte para o valor fracionario ex : [0,1] == 1/4 == 0.25
bin2frac_aux :: [Int] -> Double
bin2frac_aux [] = 0
bin2frac_aux [0] = 0
bin2frac_aux xs = 1 / fromIntegral(partfrac_bin_dec( reverse xs))

--converte para o numero inteiro referente
partfrac_bin_dec :: [Int] -> Int
partfrac_bin_dec [] = 0
partfrac_bin_dec (x:xs) = (x * 2^(length xs + 1)) + partfrac_bin_dec xs
