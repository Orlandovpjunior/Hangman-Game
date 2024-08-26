{-# LANGUAGE OverloadedStrings #-}

module GameFunctions where

import Database (atualizarPontos, atualizarDerrotas, atualizarVitorias)
import Database.MySQL.Simple (Connection)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (when, foldM)
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
loopTurnoPalavra :: Connection -> Text -> String -> String -> Int -> Int -> IO (String, Int, Int)
loopTurnoPalavra conn nomeUsuario palavra oculta erros pontos = do
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
                            loopTurnoPalavra conn nomeUsuario palavra oculta erros pontos
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

loopTurno :: Connection -> Text -> [String] -> [String] -> Int -> Int -> IO ([String], Int, Int)
loopTurno conn nomeUsuario palavras oculta erros pontos = do
    -- Função auxiliar para processar cada palavra
    let processarTurno (palavra, ocultaAtual) = do
            (novaOculta, novosErros, novosPontos) <- loopTurnoPalavra conn nomeUsuario palavra ocultaAtual erros pontos
            return (novaOculta, novosErros, novosPontos)
    
    -- Processa cada palavra usando foldM
    (ocultasFinal, errosFinal, pontosFinal) <- foldM (\(ocultas, e, p) (palavra, ocultaAtual) -> do
                                                      (novaOculta, novosErros, novosPontos) <- processarTurno (palavra, ocultaAtual)
                                                      return (ocultas ++ [novaOculta], novosErros, novosPontos))
                                                    ([], erros, pontos)
                                                    (zip palavras oculta)
    return (ocultasFinal, errosFinal, pontosFinal)


loopJogoDoisJogadores :: Connection -> Text -> Text -> [String] -> [String] -> [String] -> Int -> Int -> Int -> Int -> IO ()
loopJogoDoisJogadores conn nomeJogador1 nomeJogador2 palavras oculta1 oculta2 erros1 erros2 pontos1 pontos2 = do
    clearScreen

    -- Turno do jogador 1
    putStrLn $ "Turno de " ++ T.unpack nomeJogador1
    (novaOculta1, novosErros1, novosPontos1) <- loopTurno conn nomeJogador1 palavras oculta1 erros1 pontos1
    clearScreen

    -- Turno do jogador 2
    putStrLn $ "Turno de " ++ T.unpack nomeJogador2
    (novaOculta2, novosErros2, novosPontos2) <- loopTurno conn nomeJogador2 palavras oculta2 erros2 pontos2
    clearScreen

    -- Verifica se ambos terminaram
    if all (uncurry (==)) (zip palavras novaOculta1) && all (uncurry (==)) (zip palavras novaOculta2)
        then do
            putStrLn "Ambos os jogadores terminaram suas palavras!"
            putStrLn $ "Pontuação final de " ++ T.unpack nomeJogador1 ++ ": " ++ show novosPontos1
            putStrLn $ "Pontuação final de " ++ T.unpack nomeJogador2 ++ ": " ++ show novosPontos2
            if novosPontos1 > novosPontos2
                then putStrLn $ "Parabéns " ++ T.unpack nomeJogador1 ++ ", você venceu!"
                else if novosPontos2 > novosPontos1
                    then putStrLn $ "Parabéns " ++ T.unpack nomeJogador2 ++ ", você venceu!"
                    else putStrLn "Empate!"
        else loopJogoDoisJogadores conn nomeJogador1 nomeJogador2 palavras novaOculta1 novaOculta2 novosErros1 novosErros2 novosPontos1 novosPontos2
