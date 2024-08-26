{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (bracket)

-- Conectar ao banco de dados
conectarBanco :: IO Connection
conectarBanco = connect
    defaultConnectInfo { connectHost = "autorack.proxy.rlwy.net"
                       , connectUser = "root"
                       , connectPassword = "vwUBStNRhctaVJejQuzCZxFhowguOcFS"
                       , connectDatabase = "railway"
                       , connectPort = 41717
                       }

-- Atualizar o número de derrotas do usuário
atualizarDerrotas :: Connection -> Text -> IO ()
atualizarDerrotas conn nomeUsuario = do
    let sql = "UPDATE usuarios SET derrotas = derrotas + 1 WHERE nome = ?"
    execute conn sql (Only nomeUsuario)
    putStrLn "Derrota registrada com sucesso!"

-- Atualizar o número de vitórias do usuário
atualizarVitorias :: Connection -> Text -> IO ()
atualizarVitorias conn nomeUsuario = do
    let sql = "UPDATE usuarios SET vitorias = vitorias + 1 WHERE nome = ?"
    execute conn sql (Only nomeUsuario)
    putStrLn "Vitória registrada com sucesso!"

-- Exibir a classificação dos usuários com numeração e alinhamento
exibirClassificacao :: Connection -> IO ()
exibirClassificacao conn = do
    resultados <- query_ conn "SELECT nome, pontos, vitorias, derrotas FROM usuarios ORDER BY pontos DESC" :: IO [(Text, Int, Int, Int)]
    putStrLn "Classificação dos Usuários:"
    putStrLn $ T.unpack $ T.intercalate "  " ["Posição", "Nome", "Pontos", "Vitórias", "Derrotas"]
    putStrLn $ replicate 60 '='
    mapM_ (uncurry printResultado) (zip [1..] resultados)
  where
    printResultado pos (nome, pontos, vitorias, derrotas) = do
        let posStr = show pos ++ "º"
        let nomeStr = T.unpack nome
        let pontosStr = show pontos
        let vitoriasStr = show vitorias
        let derrotasStr = show derrotas
        putStrLn $ alignLeft 8 posStr ++ alignLeft 15 nomeStr ++ alignRight 10 pontosStr ++ alignRight 10 vitoriasStr ++ alignRight 10 derrotasStr

    alignLeft n str = str ++ replicate (n - length str) ' '
    alignRight n str = replicate (n - length str) ' ' ++ str

-- Atualizar a pontuação do usuário
atualizarPontos :: Connection -> Text -> Int -> IO ()
atualizarPontos conn nomeUsuario pontos = do
    let sql = "UPDATE usuarios SET pontos = pontos + ? WHERE nome = ?"
    execute conn sql (pontos, nomeUsuario)
    putStrLn "Pontuação atualizada com sucesso!"
