module Blackjack
( EstadoJogo
, Baralho
, converte
, tamanho
, geraBaralho
, inicializa
, inicializaAleatorio
, terminado
, distribuirCartas
, atualizaUser
, pontosUser
, pontosCasa
, mostraCartas
, mostraCreditos
, simulaRondaCasa
, terminaEj
, terminaResultado
, mostraEjinicial
) where

type Baralho = [String]

converte :: [String] -> Baralho
converte = id

tamanho :: Baralho -> Int
tamanho = length

geraBaralho :: (Ord n, Num n) => n -> [String]
geraBaralho n = if n > 1 then geraBaralhoAux ++ geraBaralho (n - 1) else geraBaralhoAux

geraBaralhoAux :: [String]
geraBaralhoAux = [ [valor, naipe] | naipe <- naipes, valor <- valores ]
    where naipes = "SHDC"
          valores = "A23456789TJQK"

valor :: (Num a, Read a) => Char -> a
valor carta
    | carta == 'A' = 11
    | carta `elem` ['2'..'9'] = read [carta]
    | otherwise = 10

data EstadoJogo = EstadoJogo
                  {
                    creditos      :: Int    ,
                    baralho       :: Baralho,
                    cartasJogador :: Baralho,
                    cartasCasa    :: Baralho
                  }

instance Show EstadoJogo where
    show :: EstadoJogo -> String
    show (EstadoJogo c _ cj cc) =
      "jogador: " ++ show cj ++ "\n" ++
      "casa: " ++ show cc ++ "\n" ++
      "creditos: " ++ show c

inicializa :: Baralho -> EstadoJogo
inicializa baralhoInicial = EstadoJogo {creditos = 100, baralho = baralhoInicial,
                                        cartasJogador = [], cartasCasa = []}

inicializaAleatorio :: Int -> Baralho -> EstadoJogo 
inicializaAleatorio c b = EstadoJogo {creditos = c, baralho = b,
                                      cartasJogador = [], cartasCasa = []}

terminado :: EstadoJogo -> Bool
terminado ej = creditos ej < 1 || tamanho (baralho ej) <= 20

somaCartas :: Baralho -> Int
somaCartas baralho = somaCartasAs baralho (ocorrenciasAs baralho) (somaCartasAux baralho)

somaCartasAux :: Baralho -> Int
somaCartasAux baralho = sum [ valor (head carta) | carta <- baralho ]

somaCartasAs :: (Ord t1, Ord t2, Num t1, Num t2) => t3 -> t2 -> t1 -> t1
somaCartasAs baralho n acc = if acc > 21 && n > 0 then somaCartasAs baralho (n-1) (acc-10) else acc

ocorrenciasAs :: Baralho -> Int
ocorrenciasAs = foldr (\x acc -> if head x == 'A' then acc + 1 else acc) 0

atualizaEj :: Int -> Baralho -> Baralho -> Baralho -> EstadoJogo
atualizaEj c b cj cc = EstadoJogo{creditos = c, baralho = b, cartasJogador = cj, cartasCasa = cc}

distribuirCartas :: EstadoJogo -> Int -> EstadoJogo
distribuirCartas ej gamba = atualizaEj (creditos ej - gamba) (drop 4 (baralho ej)) (take 2 (baralho ej)) (drop 2 (take 4 (baralho ej)))

atualizaUser :: EstadoJogo -> EstadoJogo
atualizaUser ej = atualizaEj (creditos ej) (drop 1 (baralho ej)) (cartasJogador ej ++ [head $ baralho ej]) (cartasCasa ej)

atualizaCasa :: EstadoJogo -> EstadoJogo
atualizaCasa ej = atualizaEj (creditos ej) (drop 1 (baralho ej)) (cartasJogador ej) (cartasCasa ej ++ [head $ baralho ej])

pontosUser :: EstadoJogo -> Int
pontosUser ej = somaCartas $ cartasJogador ej

pontosCasa :: EstadoJogo -> Int
pontosCasa ej = somaCartas $ cartasCasa ej

mostraCartas :: EstadoJogo -> [String]
mostraCartas ej = "jogador:" :unwords(cartasJogador ej) :"\ncasa:" :[unwords(cartasCasa ej)] 
                
mostraCreditos :: EstadoJogo -> Int
mostraCreditos = creditos

mostraEjinicial :: EstadoJogo -> String
mostraEjinicial ej = "cartas: " ++ show (tamanho (baralho ej)) ++ 
                     "\ncreditos: " ++ show (creditos ej)

simulaRondaCasa :: Int -> EstadoJogo -> EstadoJogo
simulaRondaCasa c ej                              
  | pontosCasa ej < 17 = simulaRondaCasa c (atualizaCasa ej)
  | otherwise = ej

terminaEj :: Int -> EstadoJogo -> EstadoJogo
terminaEj c ej = atualizaEj novosCreditos (baralho ej) [] []
    where novosCreditos = creditos ej + creditosResultantes c (terminaResultado ej)

terminaResultado :: EstadoJogo -> String
terminaResultado ej 
    | pontosCasa ej > 21 || (pontosUser ej <= 21 && pontosUser ej > pontosCasa ej) = "Vitoria"
    | pontosCasa ej == pontosUser ej = "Empate"
    | otherwise = "Derrota"

creditosResultantes :: Int -> String -> Int
creditosResultantes c resultado = case resultado of
                "Vitoria" -> 2 * c
                "Derrota" -> 0
                _ -> c 
