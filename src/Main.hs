{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (when)
import Data.List (intersperse)
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

-- Função principal que começa o jogo com dois jogadores
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