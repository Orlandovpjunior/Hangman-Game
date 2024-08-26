{-# LANGUAGE OverloadedStrings #-}

module GameFunctions where

import Database (atualizarPontos, atualizarDerrotas, atualizarVitorias)
import Database.MySQL.Simple (Connection)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (when)
import Data.Char (toLower)
import System.Console.ANSI (clearScreen)

-- Representações do boneco
boneco :: [String]
boneco =
    [ "  _____\n |     |\n |\n |\n |\n |"
    , "  _____\n |     |\n |     O\n |\n |\n |"
    , "  _____\n |     |\n |     O\n |     |\n |\n |"
    , "  _____\n |     |\n |     O\n |    /|\n |\n |"
    , "  _____\n |     |\n |     O\n |    /|\\\n |\n |"
    , "  _____\n |     |\n |     O\n |    /|\\\n |    /\n |"
    , "  _____\n |     |\n |     O\n |    /|\\\n |    / \\\n |"
    ]

-- Função para inicializar a palavra oculta com '_'
inicializarOculta :: String -> String
inicializarOculta palavra = replicate (length palavra) '_'

-- Função para atualizar a palavra oculta com base na letra adivinhada
atualizarOculta :: String -> Char -> String -> String
atualizarOculta palavra palpite oculta =
    [ if p == palpite then p else o | (p, o) <- zip palavra oculta ]

-- Função para formatar a palavra oculta com espaços entre os caracteres
formatarOculta :: String -> String
formatarOculta oculta = unwords (map (:[]) oculta)

-- Função para mostrar o boneco
mostrarBoneco :: Int -> IO ()
mostrarBoneco erros = putStrLn $ boneco !! min erros (length boneco - 1)

-- Função para o turno de cada jogador
loopTurno :: Connection -> Text -> String -> String -> Int -> Int -> IO (String, Int, Int)
loopTurno conn nomeUsuario palavra oculta erros pontos = do
    if erros >= 6
        then do
            mostrarBoneco erros
            putStrLn $ "Você perdeu! A palavra era: " ++ palavra
            atualizarPontos conn nomeUsuario (-pontos)
            atualizarDerrotas conn nomeUsuario
            return (oculta, erros, pontos)
        else do
            mostrarBoneco erros
            putStrLn $ "Palavra: " ++ formatarOculta oculta
            putStrLn $ "Erros: " ++ show erros
            putStr "Adivinhe uma letra: "
            palpite <- getChar
            _ <- getLine -- Para consumir o caractere de nova linha após a entrada

            if palpite `elem` palavra
                then do
                    let novaOculta = atualizarOculta palavra palpite oculta
                    if novaOculta == oculta
                        then do
                            putStrLn "Você já adivinhou essa letra. Tente outra."
                            loopTurno conn nomeUsuario palavra oculta erros pontos
                        else do
                            putStrLn "Acertou!"
                            let novosPontos = pontos + 30
                            atualizarPontos conn nomeUsuario 30
                            if novaOculta == palavra
                                then do
                                    putStrLn $ "Parabéns! Você ganhou! A palavra era: " ++ palavra
                                    atualizarVitorias conn nomeUsuario
                                    return (novaOculta, erros, novosPontos)
                                else return (novaOculta, erros, novosPontos)
                else do
                    putStrLn "Letra errada!"
                    let novosErros = erros + 1
                    let novosPontos = pontos - 10
                    atualizarPontos conn nomeUsuario (-10)
                    return (oculta, novosErros, novosPontos)

loopJogoDoisJogadores :: Connection -> Text -> Text -> String -> String -> String -> Int -> Int -> Int -> Int -> IO ()
loopJogoDoisJogadores conn nomeJogador1 nomeJogador2 palavra oculta1 oculta2 erros1 erros2 pontos1 pontos2 = do
    clearScreen

    -- Turno do jogador 1
    putStrLn $ "Turno do Jogador 1: " ++ T.unpack nomeJogador1
    (novaOculta1, novosErros1, novosPontos1) <- loopTurno conn nomeJogador1 palavra oculta1 erros1 pontos1

    -- Verifica se o jogador 1 ganhou ou perdeu
    if novaOculta1 == palavra || novosErros1 >= 6
        then do
            if novaOculta1 == palavra
                then do
                    putStrLn $ T.unpack nomeJogador1 ++ " venceu!"
                    atualizarVitorias conn nomeJogador1 -- Registra vitória do jogador 1
                    atualizarDerrotas conn nomeJogador2 -- Registra derrota do jogador 2
                else do
                    putStrLn $ T.unpack nomeJogador1 ++ " perdeu! A palavra era: " ++ palavra
                    atualizarDerrotas conn nomeJogador1 -- Registra derrota do jogador 1
                    atualizarVitorias conn nomeJogador2 -- Registra vitória do jogador 2
            return ()  -- Finaliza o jogo se o jogador 1 ganhou ou perdeu
        else do
            clearScreen
            -- Turno do jogador 2
            putStrLn $ "Turno do Jogador 2: " ++ T.unpack nomeJogador2
            (novaOculta2, novosErros2, novosPontos2) <- loopTurno conn nomeJogador2 palavra oculta2 erros2 pontos2

            -- Verifica se o jogador 2 ganhou ou perdeu
            if novaOculta2 == palavra || novosErros2 >= 6
                then do
                    if novaOculta2 == palavra
                        then do
                            putStrLn $ T.unpack nomeJogador2 ++ " venceu!"
                            atualizarVitorias conn nomeJogador2 -- Registra vitória do jogador 2
                            atualizarDerrotas conn nomeJogador1 -- Registra derrota do jogador 1
                        else do
                            putStrLn $ T.unpack nomeJogador2 ++ " perdeu! A palavra era: " ++ palavra
                            atualizarDerrotas conn nomeJogador2 -- Registra derrota do jogador 2
                            atualizarVitorias conn nomeJogador1 -- Registra vitória do jogador 1
                    return ()  -- Finaliza o jogo se o jogador 2 ganhou ou perdeu
                else loopJogoDoisJogadores conn nomeJogador1 nomeJogador2 palavra novaOculta1 novaOculta2 novosErros1 novosErros2 novosPontos1 novosPontos2
