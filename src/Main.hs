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
jogar :: Connection -> Text -> String -> IO ()
jogar conn nomeUsuario palavra = do
    let oculta = inicializarOculta palavra
    loopJogo conn nomeUsuario palavra oculta 0 0

-- Função do loop principal do jogo
loopJogo :: Connection -> Text -> String -> String -> Int -> Int -> IO ()
loopJogo conn nomeUsuario palavra oculta erros pontos = do
    if erros >= 6
        then do
            mostrarBoneco erros
            putStrLn $ "Você perdeu! A palavra era: " ++ palavra
            atualizarPontos conn nomeUsuario (-pontos) -- Deduz pontos por erros
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
                        else do
                            putStrLn "Acertou!"
                            let novosPontos = pontos + 30
                            atualizarPontos conn nomeUsuario 30 -- Atualiza banco com 30 pontos
                            let novaOcultaEstado = if novaOculta == oculta then oculta else novaOculta
                            if novaOcultaEstado == palavra
                                then putStrLn $ "Parabéns! Você ganhou! A palavra era: " ++ palavra
                                else loopJogo conn nomeUsuario palavra novaOcultaEstado erros novosPontos
                else do
                    putStrLn "Letra errada!"
                    let novosErros = erros + 1
                    let novosPontos = pontos - 10
                    atualizarPontos conn nomeUsuario (-10) -- Atualiza banco com -10 pontos
                    loopJogo conn nomeUsuario palavra oculta novosErros novosPontos

-- Conectar ao banco de dados
conectarBanco :: IO Connection
conectarBanco = connect
    defaultConnectInfo { connectHost = "autorack.proxy.rlwy.net"
                       , connectUser = "root"
                       , connectPassword = "vwUBStNRhctaVJejQuzCZxFhowguOcFS"
                       , connectDatabase = "railway"
                       , connectPort = 41717
                       }

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

-- Função auxiliar para alinhar à esquerda com um tamanho fixo
alignLeft :: Int -> String -> String
alignLeft n s = s ++ replicate (n - length s) ' '

-- Função auxiliar para alinhar à direita com um tamanho fixo
alignRight :: Int -> String -> String
alignRight n s = replicate (n - length s) ' ' ++ s

-- Adicionar um usuário
adicionarUsuario :: Connection -> Text -> Int -> Int -> Int -> IO ()
adicionarUsuario conn nome pontos vitorias derrotas = do
    let sql = "INSERT INTO usuarios (nome, pontos, vitorias, derrotas) VALUES (?, ?, ?, ?)"
    execute conn sql (nome, pontos, vitorias, derrotas)
    putStrLn $ "Usuário " ++ T.unpack nome ++ " adicionado com sucesso!"

-- Atualizar pontos do usuário
atualizarPontos :: Connection -> Text -> Int -> IO ()
atualizarPontos conn nomeUsuario pontos = do
    let sql = "UPDATE usuarios SET pontos = pontos + ? WHERE nome = ?"
    result <- execute conn sql (pontos, nomeUsuario)
    if result > 0
        then putStrLn "Pontos atualizados com sucesso!"
        else putStrLn "Nenhum usuário encontrado com o nome fornecido."

-- Função principal que começa o jogo
main :: IO ()
main = do
    conn <- conectarBanco
    exibirClassificacao conn
    putStrLn "Bem-vindo ao jogo da Forca!"
    putStrLn "Digite o nome do jogador: "
    nomeUsuario <- T.pack <$> getLine
    putStrLn "Digite a palavra para o jogo: "
    palavra <- getLine
    let palavraLimpa = filter (/= ' ') (map toLower palavra) -- Remove espaços e converte para minúsculas
    when (null palavraLimpa) $ do
        putStrLn "A palavra não pode ser vazia. Tente novamente."
        main
    jogar conn nomeUsuario palavraLimpa
    close conn