# Programação Funcional - Trabalho 1(2022/2)

# Integrantes:
Alison Schemitt , Cassiano Flores , Leonardo Cruz , Mateus de Carvalho

# Objetivo:
O objetivo deste trabalho é consolidar o conhecimento sobre conceitos e construção de funções recursivas em Haskell.

# Enunciado:
Estamos  interessados na  implementação  de  diversas  funções  de  manipulação  de “valores  binários”.Em  todos  os exercícios que lidam com representação em binário, considere queum número binário éarmazenado comouma lista de zeros e uns. Utilize as listas doHaskell nestas soluções: [Int].Para cada exercício apresente também um exemplo de  chamada  da  função  implementada.Cuidado  para a  interpretação  da  codificação  binária  solicitada  em  cada exercício, sugere-se fortemente que sejam revisados os conceitos de representaçãonumérica em binários antes de realizar a atividade!

1.Definir uma função recursiva que recebe um número binário(interpretado como número inteiro sem sinal)e retorna o valor equivalenteem decimal. 𝑏𝑖𝑛2𝑑𝑒𝑐∷[𝐼𝑛𝑡]→𝐼𝑛𝑡

2.Definir uma função recursiva que recebe umnúmero decimalinteiro não-negativoe retorna o valor equivalente em binário(interpretado como número inteiro sem sinal). 𝑑𝑒𝑐2𝑏𝑖𝑛∷𝐼𝑛𝑡→[𝐼𝑛𝑡]

3.Definir  uma  função  recursiva  que  recebe  um  número  binário na  representação  decomplementode  doise retorna o valor equivalente em decimalinteiro. 𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙2𝑑𝑒𝑐∷[𝐼𝑛𝑡]→𝐼𝑛𝑡

4.Definir uma função recursiva que recebe um número decimal inteiro eretorna o valor equivalente em binário na representação de complemento de dois. 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙∷𝐼𝑛𝑡→[𝐼𝑛𝑡]

5.Definir uma função recursiva que recebe dois números binários na representação decomplemento de dois e retorna a soma binária destes dois valores. 𝑠𝑜𝑚𝑎𝑟𝑏𝑖𝑛∷[𝐼𝑛𝑡]→[𝐼𝑛𝑡]→[𝐼𝑛𝑡]

6.Definir uma função recursiva que recebe dois números binários na representação decomplemento de dois e retorna a subtração binária destes dois valores. 𝑠𝑢𝑏𝑡𝑟𝑎𝑖𝑟𝑏𝑖𝑛∷[𝐼𝑛𝑡]→[𝐼𝑛𝑡]→[𝐼𝑛𝑡]

7.Definir uma função recursiva que recebeum número fracionário decimal por parâmetro e devolve uma tupla com   dois   números   bináriosrepresentando,respectivamente,aparte   inteira(na   representação   de complemento de doiscom no máximo32 bits)e a parte fracionária(na representação de bináriofracionadocom no máximo 32 bits).𝑓𝑟𝑎𝑐2𝑏𝑖𝑛∷𝐷𝑜𝑢𝑏𝑙𝑒→([𝐼𝑛𝑡],[𝐼𝑛𝑡])

8.Definir   uma   função   recursiva   que   recebe   uma   tupla   com   dois   números   binários   representando, respectivamente, a parte inteira (na representação de complemento de doiscom no máximo32 bits) e a parte fracionária(na representação de bináriofracionadocom no máximo 32 bits), e retorna o correspondente valor fracionário decimal.𝑏𝑖𝑛2𝑓𝑟𝑎𝑐∷([𝐼𝑛𝑡],[𝐼𝑛𝑡])→𝐷𝑜𝑢𝑏𝑙𝑒