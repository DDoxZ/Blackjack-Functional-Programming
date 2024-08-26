module Testes
( prop_mao_maximo_21
, prop_creditos_jogador
, prop_mao_casa_minimo_17
, prop_mao_jogador_nao_altera
, prop_mao_jogador_max_30
, prop_creditos_mao_inicial_21
) where

import Test.QuickCheck
import Blackjack

newtype CartaValida = CV String deriving Show
newtype Aposta = Aposta Int deriving Show

instance Arbitrary CartaValida where
    arbitrary = do
        valor <- elements ["A", "2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K"]
        naipe <- elements ["S", "H", "D", "C"]
        return $ CV (valor ++ naipe)

instance Arbitrary EstadoJogo where
    arbitrary = do
        creditos <- choose (10, 200) :: Gen Int
        tamanhoBaralho <- choose (20, 301) :: Gen Int
        baralho <- geraBaralhoTeste tamanhoBaralho
        return $ inicializaAleatorio creditos (converte baralho)

instance Arbitrary Aposta where
    arbitrary = do
        value <- choose (1, 9)
        return (Aposta value)

geraBaralhoTeste :: Int -> Gen [String]
geraBaralhoTeste 0 = return []
geraBaralhoTeste n = do
    carta <- arbitrary :: Gen CartaValida
    restoDoBaralho <- geraBaralhoTeste (n - 1)
    return ((\(CV string) -> string) carta : restoDoBaralho)

-- Proriedade: Qualquer mao inicial vai ter, em qualquer caso, um maximo de 21 pontos
prop_mao_maximo_21 :: EstadoJogo -> Aposta -> Bool
prop_mao_maximo_21 ej (Aposta a) = maoJogador <= 21 || maoCasa <= 21
    where novoEj = distribuirCartas ej a
          maoJogador = pontosUser novoEj
          maoCasa = pontosCasa novoEj

-- Propriedade: Numero de creditos do jogador apos a ronda é creditos-aposta, creditos ou creditos+aposta
prop_creditos_jogador :: EstadoJogo -> Aposta -> Bool
prop_creditos_jogador ej (Aposta a) = creditosJ == creditosIniciais - a || creditosJ == creditosIniciais || creditosJ == creditosIniciais + a
    where creditosIniciais = mostraCreditos ej
          novoEj = terminaEj a . simulaRondaCasa a . distribuirCartas ej $ a  
          creditosJ = mostraCreditos novoEj

-- Propriedade: No final da vez da casa, a mao da casa tem sempre pelo menos 17 pontos
prop_mao_casa_minimo_17 :: EstadoJogo -> Aposta -> Bool
prop_mao_casa_minimo_17 ej (Aposta a) = maoCasa >= 17
    where novoEj = simulaRondaCasa a . distribuirCartas ej $ a
          maoCasa = pontosCasa novoEj

-- Propriedade: Durante a vez da casa a mao do jogador nao se altera (o valor desta)
prop_mao_jogador_nao_altera :: EstadoJogo -> Aposta -> Bool
prop_mao_jogador_nao_altera ej (Aposta a) = maoInicial == maoFinal
    where ej1 = atualizaUser . distribuirCartas ej $ a
          maoInicial = pontosUser ej1
          ej2 = simulaRondaCasa a ej1
          maoFinal = pontosUser ej2

-- Propriedade: Uma mão do jogador nunca pode passar dos 30 pontos, isto é, quando o jogador
-- tem 20 pontos, por exemplo, e faz hit, o máximo que consegue alcancar é 30 pontos, testando 
-- assim que não há cartas inválidas no deck(maiores que 11 pontos) e que a carta "As" de qualquer 
-- naipe vale o valor correto: 1 se rebentar com 11, se não, 11.
prop_mao_jogador_max_30 :: EstadoJogo -> Aposta -> Bool
prop_mao_jogador_max_30 ej (Aposta a) = pontosUser novoEj <= 30
    where novoEj = atualizaUser . distribuirCartas ej $ a

--Proriedade: Quando a mão inicial do jogador é de 21 o jogador nunca pode perder dinheiro no fim da ronda
prop_creditos_mao_inicial_21 :: EstadoJogo -> Aposta -> Property
prop_creditos_mao_inicial_21 ej (Aposta a) = pontosUser ej1 == 21 ==> c1 < c2
    where ej1 = distribuirCartas ej a
          ej2 = terminaEj a ej1
          c1 = mostraCreditos ej1
          c2 = mostraCreditos ej2
