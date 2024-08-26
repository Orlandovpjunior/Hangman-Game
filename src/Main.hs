{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database (conectarBanco, atualizarPontos, atualizarDerrotas, atualizarVitorias, exibirClassificacao)
import GameFunctions (loopJogoDoisJogadores, inicializarOculta, atualizarOculta, formatarOculta, mostrarBoneco, loopTurno)
import Database.MySQL.Simple (Connection, close)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (when)
import Data.Char (toLower)
import System.Console.ANSI (clearScreen)

main :: IO ()
main = do
    conn <- conectarBanco
    exibirClassificacao conn
    putStrLn "Bem-vindo ao jogo da Forca!"

    -- Seleção dos jogadores
    putStrLn "Digite o nome do jogador 1: "
    nomeJogador1 <- T.pack <$> getLine
    putStrLn "Digite o nome do jogador 2: "
    nomeJogador2 <- T.pack <$> getLine

    -- Seleção da palavra
    putStrLn "Digite a palavra para o jogo: "
    palavra <- getLine
    let palavraLimpa = filter (/= ' ') (map toLower palavra)
    when (null palavraLimpa) $ do
        putStrLn "A palavra não pode ser vazia. Tente novamente."
        main

    -- Inicializa a palavra oculta para ambos os jogadores
    let oculta1 = inicializarOculta palavraLimpa
    let oculta2 = inicializarOculta palavraLimpa

    -- Inicia o loop do jogo
    loopJogoDoisJogadores conn nomeJogador1 nomeJogador2 palavraLimpa oculta1 oculta2 0 0 0 0

    close conn