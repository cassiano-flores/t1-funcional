------------------------------------------------------------------------------------------------------
--  Disciplina: Programação Funcional
--  Integrantes: Alison Schemitt, Cassiano Flores, Leonardo Cruz, Mateus de Carvalho
------------------------------------------------------------------------------------------------------


-- Definir uma função recursiva que recebe um número binário(interpretado como número inteiro sem sinal) 
-- e retorna o valor equivalenteem decimal. 𝑏𝑖𝑛2𝑑𝑒𝑐∷[𝐼𝑛𝑡]→𝐼𝑛𝑡
--OK
bin2dec :: [Int] -> Int
bin2dec [] = 0
bin2dec (s:xs) = (s * 2 ^ (length xs)) + bin2dec xs

--Definir uma função recursiva que recebe umnúmero decimalinteiro não-negativoe retorna o valor equivalente 
--em binário(interpretado como número inteiro sem sinal). 𝑑𝑒𝑐2𝑏𝑖𝑛∷𝐼𝑛𝑡→[𝐼𝑛𝑡]
--OK
dec2bin :: Int -> [Int]
dec2bin 0 = [0]
dec2bin n = dec2binaux n

dec2binaux :: Int -> [Int]
dec2binaux 0 = []
dec2binaux n | n `mod` 2 == 1 = dec2binaux (n `div` 2) ++ [1]
             | n `mod` 2 == 0 = dec2binaux (n `div` 2) ++ [0]

-- Definir  uma  função  recursiva  que  recebe  um  número  binário na  representação  decomplementode  dois 
-- e retorna o valor equivalente em decimalinteiro. 𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙2𝑑𝑒𝑐∷[𝐼𝑛𝑡]→𝐼𝑛𝑡
--OK
bincompl2dec :: [Int] -> Int
bincompl2dec (s:xs) = (-1 * (s * 2 ^ (length xs))) + bin2dec xs

--Definir uma função recursiva que recebe um número decimal inteiro eretorna o valor equivalente em binário na 
--representação de complemento de dois. 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙∷𝐼𝑛𝑡→[𝐼𝑛𝑡]
--OK
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

--Definir uma função recursiva que recebe dois números binários na representação decomplemento de dois e retorna 
--a soma binária destes dois valores. 𝑠𝑜𝑚𝑎𝑟𝑏𝑖𝑛∷[𝐼𝑛𝑡]→[𝐼𝑛𝑡]→[𝐼𝑛𝑡]
--OK
somarbin :: [Int] -> [Int] -> [Int]
somarbin a b = dec2bincompl ((bincompl2dec a) + (bincompl2dec b))

--Definir uma função recursiva que recebe dois números binários na representação decomplemento de dois e retorna 
--a subtração binária destes dois valores. 𝑠𝑢𝑏𝑡𝑟𝑎𝑖𝑟𝑏𝑖𝑛∷[𝐼𝑛𝑡]→[𝐼𝑛𝑡]→[𝐼𝑛𝑡]
--OK
subtrairbin :: [Int] -> [Int] -> [Int]
subtrairbin a b = dec2bincompl ((bincompl2dec a) - (bincompl2dec b))


-- **** valores negativos ta estranho para acertar teria que normalizar e ver a questão do expoente
-- **** seguimos o enunciado onde diz - 'a parte inteira com representação de complemento de dois com no maximo 32 bis'
-- **** não está claro se queria no padrão IEEE 754


--Definir uma função recursiva que recebe um número fracionário decimal por parâmetro e devolve uma tupla com dois 
--números binários representando, respectivamente, aparte inteira(na representação de complemento de dois com 
--no máximo 32 bits)e a parte fracionária(na representação de bináriofracionado com no máximo 32 bits). 
--𝑓𝑟𝑎𝑐2𝑏𝑖𝑛∷𝐷𝑜𝑢𝑏𝑙𝑒→([𝐼𝑛𝑡],[𝐼𝑛𝑡])

frac2bin :: Double -> ([Int], [Int])
frac2bin x = if x >= 0 then ( reverse( take 32 (reverse(dec2bincompl(floor x)))), take 32 (parte_frac x))
             else ( reverse( take 32 (reverse(dec2bincompl( -1 * floor(-1*x))))), take 32 (parte_frac x))

parte_frac ::Double -> [Int]
parte_frac n  | snd( properFraction n) == 0.0 = [0]
                    | snd( properFraction n) > 0 = parte_frac_aux( snd( properFraction n))
                    | snd( properFraction n) < 0 = parte_frac_aux( snd( properFraction(-1 * n)))

parte_frac_aux :: Double -> [Int]
parte_frac_aux 0 = []
parte_frac_aux n = ([floor(n*2)] ++ parte_frac_aux (snd (properFraction (n*2))))

--Definir   uma   função   recursiva   que   recebe   uma   tupla   com   dois   números   binários   representando, respectivamente, 
--a parte inteira (na representação de complemento de doiscom no máximo32 bits) e a parte fracionária(na representação de bináriofracionadocom 
--no máximo 32 bits), e retorna o correspondente valor fracionário decimal.𝑏𝑖𝑛2𝑓𝑟𝑎𝑐∷([𝐼𝑛𝑡],[𝐼𝑛𝑡])→𝐷𝑜𝑢𝑏𝑙𝑒

bin2frac :: ([Int], [Int]) -> Double
bin2frac (a,b)  | (bincompl2dec a) >= 0 = (fromIntegral(bincompl2dec a)) + (bin2frac_aux b)
                | (bincompl2dec a) < 0 = (fromIntegral(bincompl2dec a)) - (bin2frac_aux b)

bin2frac_aux :: [Int] -> Double
bin2frac_aux [] = 0
bin2frac_aux xs = 1 / fromIntegral(partfrac_bin_dec( reverse xs))

partfrac_bin_dec :: [Int] -> Int
partfrac_bin_dec [] = 0
partfrac_bin_dec (x:xs) = (x * 2^(length xs + 1)) + partfrac_bin_dec xs

exibe_double :: Int -> Int -> Double
exibe_double a b = read ((show a) ++ "." ++ (show b))
