# Hangman-Game
O maior jogo da forca de todos os tempos
# ORIENTAÇÕES
1° rodem no terminal o comando: 
cabal install mysql-simple
Para baixar a biblioteca mysq para haskell
2° Acessem o link: https://railway.app/invite/JE4wkevQsaw
Esse link é para ter acesso ao banco online, ele já da permissão para editar o banco. Ai tem todas as informações de como se conectar e (se quiserem) podem editar diretamente por ele.
3° Conexão com o banco em haskell (já ta tudo preenchido) Façam um teste de conexão colocando um print para exibir na tela em caso de conexão:
{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T

-- Conectar ao banco de dados
conectarBanco :: IO Connection
conectarBanco = connect
    defaultConnectInfo { connectHost = "autorack.proxy.rlwy.net"
                       , connectUser = "root"
                       , connectPassword = "vwUBStNRhctaVJejQuzCZxFhowguOcFS"
                       , connectDatabase = "railway"
                       , connectPort = 41717
                       }
Ex teste:
main :: IO ()
main = do
    conn <- conectarBanco
    adicionarUsuario conn "João" 100 5 2
    exibirClassificacao conn
    atualizarPontos conn 1 50
    close conn
4° Editor de código SQL (https://www.youtube.com/watch?v=Z84HYnPf2TA)
Vocês também podem conectar o serviço do banco em um editor específo para SQL, esse editor tbm pode se conectar com o banco e vocês podem criar as tabelas diretamente por ele. Recomendo esse beekeper studio que foi utilizado nesse vídeo.
