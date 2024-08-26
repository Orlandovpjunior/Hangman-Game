{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database (conectarBanco, exibirClassificacao, escolherTema, escolherPalavras)
import GameFunctions (inicializarOculta, loopJogoDoisJogadores)
import Database.MySQL.Simple (close)
import Data.Text (Text)
import qualified Data.Text as T

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

    -- Seleção do tema
    temaEscolhido <- escolherTema conn
    palavras <- escolherPalavras conn temaEscolhido

    if length palavras == 3
        then do
            -- Inicializa as palavras ocultas para ambos os jogadores
            let oculta1 = map inicializarOculta palavras
            let oculta2 = map inicializarOculta palavras

            -- Inicia o loop do jogo
            loopJogoDoisJogadores conn nomeJogador1 nomeJogador2 palavras oculta1 oculta2 0 0 0 0

        else putStrLn "Erro: Não foi possível iniciar o jogo."

    close conn
