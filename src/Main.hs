{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (when)
import Data.List (intersperse)
import Data.Char (toLower)

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

-- Função principal do jogo da forca
jogar :: String -> IO ()
jogar palavra = do
    let oculta = inicializarOculta palavra
    loopJogo palavra oculta 0

-- Função do loop principal do jogo
loopJogo :: String -> String -> Int -> IO ()
loopJogo palavra oculta erros = do
    if erros >= 6
        then do
            mostrarBoneco erros
            putStrLn $ "Você perdeu! A palavra era: " ++ palavra
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
                        then putStrLn "Você já adivinhou essa letra."
                        else putStrLn "Acertou!"
                    let novosErros = erros
                    let novaOcultaEstado = if novaOculta == oculta then oculta else novaOculta
                    if novaOcultaEstado == palavra
                        then putStrLn $ "Parabéns! Você ganhou! A palavra era: " ++ palavra
                        else loopJogo palavra novaOcultaEstado novosErros
                else do
                    putStrLn "Letra errada!"
                    let novosErros = erros + 1
                    loopJogo palavra oculta novosErros

-- Conectar ao banco de dados
conectarBanco :: IO Connection
conectarBanco = connect
    defaultConnectInfo { connectHost = "autorack.proxy.rlwy.net"
                       , connectUser = "root"
                       , connectPassword = "vwUBStNRhctaVJejQuzCZxFhowguOcFS"
                       , connectDatabase = "railway"
                       , connectPort = 41717
                       }

-- Exibir a classificação dos usuários
exibirClassificacao :: Connection -> IO ()
exibirClassificacao conn = do
    resultados <- query_ conn "SELECT nome, pontos, vitorias, derrotas FROM usuarios ORDER BY pontos DESC" :: IO [(Text, Int, Int, Int)]
    putStrLn "Classificação dos Usuários:"
    putStrLn $ T.unpack $ T.intercalate " " ["Nome", "Pontos", "Vitórias", "Derrotas"]
    putStrLn $ replicate 50 '='
    mapM_ printResultado resultados
  where
    printResultado (nome, pontos, vitorias, derrotas) =
        putStrLn $ T.unpack $ T.intercalate " " [nome, T.pack (show pontos), T.pack (show vitorias), T.pack (show derrotas)]


-- Adicionar um usuário
adicionarUsuario :: Connection -> Text -> Int -> Int -> Int -> IO ()
adicionarUsuario conn nome pontos vitorias derrotas = do
    let sql = "INSERT INTO usuarios (nome, pontos, vitorias, derrotas) VALUES (?, ?, ?, ?)"
    execute conn sql (nome, pontos, vitorias, derrotas)
    putStrLn $ "Usuário " ++ T.unpack nome ++ " adicionado com sucesso!"

-- Atualizar pontos do usuário
atualizarPontos :: Connection -> Int -> Int -> IO ()
atualizarPontos conn idUsuario pontos = do
    let sql = "UPDATE usuarios SET pontos = pontos + ? WHERE id = ?"
    result <- execute conn sql (pontos, idUsuario)
    if result > 0
        then putStrLn "Pontos atualizados com sucesso!"
        else putStrLn "Nenhum usuário encontrado com o ID fornecido."



-- Função principal que começa o jogo
main :: IO ()
main = do
    conn <- conectarBanco
    adicionarUsuario conn "João" 100 5 2
    exibirClassificacao conn
    atualizarPontos conn 1 50
    close conn
    putStrLn "Bem-vindo ao jogo da Forca!"
    putStr "Digite a palavra para o jogo: "
    palavra <- getLine
    let palavraLimpa = filter (/= ' ') (map toLower palavra) -- Remove espaços e converte para minúsculas
    when (null palavraLimpa) $ do
        putStrLn "A palavra não pode ser vazia. Tente novamente."
        main
    jogar palavraLimpa

