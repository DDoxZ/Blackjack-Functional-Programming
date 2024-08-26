module Main
( main
) where

import System.Environment
import System.Directory
import System.Random
import System.Random.Shuffle
import Test.QuickCheck
import Blackjack
import Testes

main = do
    args <- getArgs
    case args of
        [] -> do
            baralho <- criaBaralho "default.bar"
            iniciaJogo baralho

        ["-t"] -> correTestes

        ["-n", n] -> do
            baralho <- criaBaralhoAleatorio $ read n
            iniciaJogo baralho --n tem de ter o tipo int

        [file] -> do
                fileExists <- doesFileExist file
                if fileExists then do
                        baralho <- criaBaralho file
                        iniciaJogo baralho
                else menuUtilizacao

        _ -> menuUtilizacao

iniciaJogo :: EstadoJogo -> IO ()
iniciaJogo ej = do
    mostraInicio ej
    if terminado ej then terminaJogo ej
    else do
        line <- getLine
        let apostaJogador = words line
        case apostaJogador of
            ["apostar", aposta] -> do
                let novoEj = distribuirCartas ej $ read aposta
                mostraMaos novoEj
                jogaUser novoEj $ read aposta
            ["sair"] -> terminaJogo ej

jogaUser :: EstadoJogo -> Int -> IO ()
jogaUser ej aposta = do
    if pontosUser ej == 21 then jogaCasa ej aposta
    else if pontosUser ej > 21 then terminaRonda ej aposta
    else do
        line <- getLine
        let instrucao = head $ words line
        case instrucao of
            "stand" -> jogaCasa ej aposta
            "hit"   -> do
                let novoEj = atualizaUser ej
                mostraMaos novoEj
                jogaUser novoEj aposta

jogaCasa :: EstadoJogo -> Int -> IO ()
jogaCasa ej aposta = do
    let novoEj = simulaRondaCasa aposta ej
    mostraMaos novoEj
    terminaRonda novoEj aposta

terminaRonda :: EstadoJogo -> Int -> IO ()
terminaRonda ej aposta = do
    putStrLn $ terminaResultado ej
    let novoEj = terminaEj aposta ej
    iniciaJogo novoEj

terminaJogo :: EstadoJogo -> IO ()
terminaJogo ej = putStrLn $ "saldo final: " ++ show (mostraCreditos ej)

criaBaralho :: String -> IO EstadoJogo
criaBaralho ficheiro = do
    baralho <- readFile ficheiro
    return $ inicializa $ converte (lines baralho)

criaBaralhoAleatorio :: (Ord n, Num n) => n -> IO EstadoJogo
criaBaralhoAleatorio n = do
    let baralho = geraBaralho n
    gen <- newStdGen
    return $ inicializa $ converte (shuffle' baralho (tamanho baralho) gen)

mostraMaos :: EstadoJogo -> IO()
mostraMaos ej = putStrLn $ unwords (mostraCartas ej)

mostraInicio :: EstadoJogo -> IO()
mostraInicio ej = putStrLn $ mostraEjinicial ej

menuUtilizacao :: IO ()
menuUtilizacao = do
        putStrLn "Utilização:"
        putStrLn " ./Main ficheiro -- carrega um baralho para jogar Blackjack"
        putStrLn " ./Main          -- carrega o baralho default.bar"
        putStrLn " ./Main -n X     -- carrega um baralho aleatorio formado por X baralhos normais de cartas"
        putStrLn " ./Main -t       -- corre os testes"

correTestes :: IO()
correTestes = do
    quickCheck prop_mao_maximo_21
    quickCheck prop_creditos_jogador
    quickCheck prop_mao_casa_minimo_17
    quickCheck prop_mao_jogador_nao_altera
    quickCheck prop_mao_jogador_max_30
    quickCheck prop_creditos_mao_inicial_21
