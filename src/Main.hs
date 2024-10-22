{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toLower)
import System.Console.ANSI (clearScreen)
import System.Random (randomRIO)
import System.IO (writeFile, readFile)
import System.Exit (exitSuccess)
import Data.List (intercalate)
import Control.Monad (when)



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

-- Função para exibir letras já tentadas
exibirLetrasTentadas :: [Char] -> IO ()
exibirLetrasTentadas tentadas = putStrLn $ "Letras já tentadas: " ++ [c | c <- tentadas]

-- Função para o turno de cada jogador
loopTurno :: Connection -> Text -> String -> String -> Int -> Int -> [Char] -> IO (String, Int, Int, [Char])
loopTurno conn nomeUsuario palavra oculta erros pontos tentadas = do
    if erros >= 6
        then do
            mostrarBoneco erros
            putStrLn $ "Você perdeu! A palavra era: " ++ palavra
            atualizarPontos conn nomeUsuario (-pontos)
            atualizarDerrotas conn nomeUsuario
            return (oculta, erros, pontos, tentadas)
        else do
            mostrarBoneco erros
            putStrLn $ "Palavra: " ++ formatarOculta oculta
            putStrLn $ "Erros: " ++ show erros
            exibirLetrasTentadas tentadas
            putStr "Adivinhe uma letra: (ou digite 'pause' para pausar o jogo):  "
            input <- getLine
            let inputLower = map toLower input

            if inputLower == "pause"
                then do
                    salvarEstado "estado_jogo.txt" palavra oculta erros pontos tentadas
                    putStrLn "Jogo pausado e salvo. Você pode continuar mais tarde."
                    exitSuccess
                else do
                    let palpiteLower = toLower $ head input
                    if palpiteLower `elem` tentadas
                        then do
                            putStrLn "Você já tentou essa letra. Tente outra."
                            loopTurno conn nomeUsuario palavra oculta erros pontos tentadas
                        else if palpiteLower `elem` palavra
                            then do
                                let novaOculta = atualizarOculta palavra palpiteLower oculta
                                if novaOculta == oculta
                                    then do
                                        putStrLn "Você já adivinhou essa letra. Tente outra."
                                        loopTurno conn nomeUsuario palavra oculta erros pontos (palpiteLower : tentadas)
                                    else do
                                        putStrLn "Acertou!"
                                        let novosPontos = pontos + 30
                                        atualizarPontos conn nomeUsuario 30
                                        if novaOculta == palavra
                                            then do
                                                putStrLn $ "Parabéns! Você ganhou! A palavra era: " ++ palavra
                                                atualizarVitorias conn nomeUsuario
                                                return (novaOculta, erros, novosPontos, palpiteLower : tentadas)
                                            else return (novaOculta, erros, novosPontos, palpiteLower : tentadas)
                    else do
                        putStrLn "Letra errada!"
                        let novosErros = erros + 1
                        let novosPontos = pontos - 10
                        atualizarPontos conn nomeUsuario (-10)
                        return (oculta, novosErros, novosPontos, palpiteLower : tentadas)

-- Função principal que começa o jogo com dois jogadores e registra o confronto
confrontoEntreJogadores :: Text -> Text -> IO ()
confrontoEntreJogadores nomeJogador1 nomeJogador2 = do
    conn <- conectarBanco

    -- Define as palavras e as condições iniciais do jogo
    let palavras = ["palavra1", "palavra2", "palavra3"]  -- Substitua por palavras reais
        oculta1 = replicate (length (head palavras)) '_'  -- Substitua por palavra oculta inicial para o jogador 1
        oculta2 = replicate (length (palavras !! 1)) '_'  -- Substitua por palavra oculta inicial para o jogador 2
        erros1 = 0
        erros2 = 0
        pontos1 = 0
        pontos2 = 0
        tentadas1 = []
        tentadas2 = []

    -- Inicia o loop do jogo entre dois jogadores
    loopJogoDoisJogadores conn nomeJogador1 nomeJogador2 palavras oculta1 oculta2 erros1 erros2 pontos1 pontos2 tentadas1 tentadas2

    -- Fecha a conexão com o banco de dados
    close conn


-- Função principal que começa o jogo com dois jogadores
loopJogoDoisJogadores :: Connection -> Text -> Text -> [String] -> String -> String -> Int -> Int -> Int -> Int -> [Char] -> [Char] -> IO ()
loopJogoDoisJogadores conn nomeJogador1 nomeJogador2 [palavra1, palavra2, palavra3] oculta1 oculta2 erros1 erros2 pontos1 pontos2 tentadas1 tentadas2 = do
    clearScreen

    -- Turno do jogador 1
    putStrLn $ "Turno do Jogador 1: " ++ T.unpack nomeJogador1
    (novaOculta1, novosErros1, novosPontos1, novasTentativas1) <- loopTurno conn nomeJogador1 palavra1 oculta1 erros1 pontos1 tentadas1

    -- Verifica se o jogador 1 ganhou ou perdeu
    if novaOculta1 == palavra1 || novosErros1 >= 6
        then do
            if novaOculta1 == palavra1
                then do
                    putStrLn $ T.unpack nomeJogador1 ++ " venceu!"
                    atualizarVitorias conn nomeJogador1 -- Registra vitória do jogador 1
                    atualizarDerrotas conn nomeJogador2 -- Registra derrota do jogador 2
                    registrarConfronto conn nomeJogador1 nomeJogador2 nomeJogador1 -- Registra confronto
                else do
                    putStrLn $ T.unpack nomeJogador1 ++ " perdeu! A palavra era: " ++ palavra1
                    atualizarDerrotas conn nomeJogador1 -- Registra derrota do jogador 1
                    atualizarVitorias conn nomeJogador2 -- Registra vitória do jogador 2
                    registrarConfronto conn nomeJogador1 nomeJogador2 nomeJogador2 -- Registra confronto
            return ()  -- Finaliza o jogo se o jogador 1 ganhou ou perdeu
        else do
            clearScreen
            -- Turno do jogador 2
            putStrLn $ "Turno do Jogador 2: " ++ T.unpack nomeJogador2
            (novaOculta2, novosErros2, novosPontos2, novasTentativas2) <- loopTurno conn nomeJogador2 palavra2 oculta2 erros2 pontos2 tentadas2

            -- Verifica se o jogador 2 ganhou ou perdeu
            if novaOculta2 == palavra2 || novosErros2 >= 6
                then do
                    if novaOculta2 == palavra2
                        then do
                            putStrLn $ T.unpack nomeJogador2 ++ " venceu!"
                            atualizarVitorias conn nomeJogador2 -- Registra vitória do jogador 2
                            atualizarDerrotas conn nomeJogador1 -- Registra derrota do jogador 1
                            registrarConfronto conn nomeJogador1 nomeJogador2 nomeJogador2 -- Registra confronto
                        else do
                            putStrLn $ T.unpack nomeJogador2 ++ " perdeu! A palavra era: " ++ palavra2
                            atualizarDerrotas conn nomeJogador2 -- Registra derrota do jogador 2
                            atualizarVitorias conn nomeJogador1 -- Registra vitória do jogador 1
                            registrarConfronto conn nomeJogador1 nomeJogador2 nomeJogador1 -- Registra confronto
                    return ()  -- Finaliza o jogo se o jogador 2 ganhou ou perdeu
                else loopJogoDoisJogadores conn nomeJogador1 nomeJogador2 [palavra1, palavra2, ""] novaOculta1 novaOculta2 novosErros1 novosErros2 novosPontos1 novosPontos2 novasTentativas1 novasTentativas2


-- Função para registrar o confronto no banco de dados
registrarConfronto :: Connection -> Text -> Text -> Text -> IO ()
registrarConfronto conn nomeJogador1 nomeJogador2 vencedorNome = do
    -- Insere o confronto na tabela Confronto
    let queryStr = "INSERT INTO Confronto (jogador1_nome, jogador2_nome, vencedor_nome) VALUES (?, ?, ?)"
    execute conn queryStr (nomeJogador1, nomeJogador2, vencedorNome)
    putStrLn "Confronto registrado com sucesso!"

-- Função para obter o histórico de confrontos entre dois jogadores
obterHistoricoEntreJogadores :: Connection -> Text -> Text -> IO ()
obterHistoricoEntreJogadores conn nomeJogador1 nomeJogador2 = do
    -- Consulta para obter o histórico de vitórias e derrotas entre os dois jogadores
    resultados <- query conn 
        "SELECT \
        \ SUM(CASE WHEN c.vencedor_nome = ? THEN 1 ELSE 0 END) AS vitorias, \
        \ SUM(CASE WHEN c.vencedor_nome <> ? THEN 1 ELSE 0 END) AS derrotas \
        \ FROM Confronto c \
        \ WHERE (c.jogador1_nome = ? AND c.jogador2_nome = ?) OR (c.jogador1_nome = ? AND c.jogador2_nome = ?)"
        (nomeJogador1, nomeJogador1, nomeJogador1, nomeJogador2, nomeJogador2, nomeJogador1) :: IO [(Int, Int)]
    
    -- Exibe o histórico entre os dois jogadores
    case resultados of
        [(vitorias, derrotas)] -> do
            putStrLn $ T.unpack nomeJogador1 ++ " vs " ++ T.unpack nomeJogador2 ++ ":"
            putStrLn $ show vitorias ++ " vitórias, " ++ show derrotas ++ " derrotas"
        _ -> putStrLn "Erro ao recuperar o histórico."

    
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

-- Atualizar a pontuação do usuário
atualizarPontos :: Connection -> Text -> Int -> IO ()
atualizarPontos conn nomeUsuario pontos = do
    let sql = "UPDATE usuarios SET pontos = pontos + ? WHERE nome = ?"
    execute conn sql (pontos, nomeUsuario)
    putStrLn "Pontuação atualizada com sucesso!"

-- Exibir a classificação dos usuários com numeração e alinhamento
exibirClassificacao :: Connection -> IO ()
exibirClassificacao conn = do
    resultados <- query_ conn "SELECT nome, pontos, vitorias, derrotas FROM usuarios ORDER BY pontos DESC" :: IO [(Text, Int, Int, Int)]
    putStrLn "Classificação dos Usuários:"
    putStrLn $ T.unpack $ T.intercalate "  " ["Posição", "Nome", "Pontos", "Vitórias", "Derrotas"]
    putStrLn $ replicate 50 '-'
    mapM_ imprimirResultado (zip [1..] resultados)
  where
    imprimirResultado (pos, (nome, pontos, vitorias, derrotas)) = do
        let posStr = show pos ++ "º"
        let nomeStr = T.unpack nome
        let pontosStr = show pontos
        let vitoriasStr = show vitorias
        let derrotasStr = show derrotas
        putStrLn $ alignLeft 8 posStr ++ alignLeft 15 nomeStr ++ alignRight 10 pontosStr ++ alignRight 10 vitoriasStr ++ alignRight 10 derrotasStr

    alignLeft n str = str ++ replicate (n - length str) ' '
    alignRight n str = replicate (n - length str) ' ' ++ str

-- Função para listar os temas disponíveis
listarTemas :: Connection -> IO [(Int, Text)]
listarTemas conn = do
    temas <- query_ conn "SELECT id, nome FROM temas" :: IO [(Int, Text)]
    return temas

-- Função para selecionar palavras por tema
selecionarPalavrasPorTema :: Connection -> Int -> IO [String]
selecionarPalavrasPorTema conn temaId = do
    palavras <- query conn "SELECT palavra FROM palavras WHERE tema_id = ?" (Only temaId) :: IO [Only Text]
    return [T.unpack p | Only p <- palavras]

-- Função para escolher n palavras aleatórias distintas
escolherPalavrasAleatorias :: [String] -> Int -> IO [String]
escolherPalavrasAleatorias palavras n = do
    indices <- randomIndices (length palavras) n
    return [palavras !! i | i <- indices]

-- Função auxiliar para gerar índices aleatórios únicos
randomIndices :: Int -> Int -> IO [Int]
randomIndices maxVal count = go count []
  where
    go 0 acc = return acc
    go n acc = do
        i <- randomRIO (0, maxVal - 1)
        if i `elem` acc
            then go n acc
            else go (n - 1) (i : acc)

-- Função para criar um novo usuário
criaUsuario :: Connection -> Text -> IO()
criaUsuario conn nickname = do
    exists <- readUsuario conn nickname

    if exists
        then do
            resultados <- query_ conn "SELECT nome, pontos, vitorias, derrotas FROM usuarios ORDER BY pontos DESC" :: IO [(Text, Int, Int, Int)]
            print("Jogador selecionado")
        else do
            let sql = "INSERT INTO usuarios (nome, pontos, vitorias, derrotas) VALUES (?, 0, 0, 0)"
            execute conn sql (Only nickname)
            print("Usuario criado com sucesso!")
            
-- Função auxiliar para verificar se um usuário com o nickname especificado existe
readUsuario :: Connection -> Text -> IO Bool
readUsuario conn nickname = do
    let sql = "SELECT COUNT(1) FROM usuarios WHERE nome = ?"
    count <- query conn sql (Only nickname) :: IO [Only Int]
    return $ case count of
        [Only c] -> c > 0
        _        -> False

-- Função para calcular e exibir o desempenho dos usuários
displayPerformance :: Connection -> IO ()
displayPerformance conn = do
    results <- query_ conn "SELECT nome, vitorias, derrotas, pontos FROM usuarios" :: IO [(Text, Int, Int, Int)]
    mapM_ printPerformance results
  where
    printPerformance (nome, vitorias, derrotas, pontuacao) = do
        let victories = vitorias
        let losses = derrotas
        let points = pontuacao
        let totalGames = victories + losses
        let accuracy = if totalGames > 0 then (fromIntegral victories / fromIntegral totalGames) * 100 else 0
        putStrLn $ "Jogador: " ++ T.unpack nome
        putStrLn $ "Vitórias: " ++ show victories
        putStrLn $ "Derrotas: " ++ show losses
        putStrLn $ "Pontos: " ++ show points
        putStrLn $ "Percentual de Acerto: " ++ show accuracy ++ "%"
        putStrLn "-------------------------"

-- Função para calcular e exibir o desempenho de um jogador específico
displayPlayerPerformance :: Connection -> Text -> IO ()
displayPlayerPerformance conn playerName = do
    -- Consulta SQL com um parâmetro para o nome do jogador
    results <- query conn
        "SELECT nome, pontos, vitorias, derrotas FROM usuarios WHERE nome = ?"
        (Only playerName) :: IO [(Text, Int, Int, Int)]
    mapM_ printPerformance results
  where
    printPerformance (nome, pontos, vitorias, derrotas) = do
        let victories = vitorias
        let losses = derrotas
        let points = pontos
        let totalGames = victories + losses
        let accuracy = if totalGames > 0 then (fromIntegral victories / fromIntegral totalGames) * 100 else 0
        putStrLn $ "Jogador: " ++ T.unpack nome
        putStrLn $ "Pontos: " ++ show points
        putStrLn $ "Vitórias: " ++ show victories
        putStrLn $ "Derrotas: " ++ show losses
        putStrLn $ "Percentual de Acerto: " ++ show accuracy ++ "%"
        putStrLn "-------------------------"

-- Função para selecionar palavras com base na dificuldade
selecionarPalavrasPorDificuldade :: Connection -> String -> IO [String]
selecionarPalavrasPorDificuldade conn dificuldade = do
    putStrLn $ "Consultando palavras com dificuldade: " ++ dificuldade
    -- Consulta as palavras com base na dificuldade
    palavras <- query conn
        "SELECT palavra FROM palavras WHERE dificuldade = ?"
        (Only (T.pack dificuldade)) :: IO [Only T.Text]
    return [T.unpack p | Only p <- palavras]

-- Função para salvar o estado do jogo em um arquivo
salvarEstado :: FilePath -> String -> String -> Int -> Int -> String -> IO ()
salvarEstado caminho palavra oculta erros pontos tentadas = do
    let tentadasString = [c | c <- tentadas] 
    let conteudo = unlines [palavra, oculta, show erros, show pontos, tentadasString]
    writeFile caminho conteudo

-- Função para carregar o estado do jogo a partir de um arquivo
carregarEstado :: FilePath -> IO (String, String, Int, Int, [Char])
carregarEstado caminho = do
    conteudo <- readFile caminho
    let [palavra, oculta, errosStr, pontosStr, tentadasStr] = lines conteudo
    return ( palavra
           , oculta
           , read errosStr
           , read pontosStr
           , tentadasStr
           )

-- Função principal que começa o jogo com dois jogadores
main :: IO ()
main = do
    conn <- conectarBanco
    exibirClassificacao conn
    putStrLn "Bem-vindo ao jogo da Forca!"

    -- Seleção de continuar um jogo pausado ou iniciar um novo
    putStrLn "Deseja continuar um jogo pausado? (s/n)"
    escolha <- getLine
    if escolha == "s"
        then do
            (palavra, oculta, erros, pontos, tentadas) <- carregarEstado "estado_jogo.txt"
            putStrLn "Continuando o jogo pausado..."
            -- Atribuindo o nome do jogador de maneira estática para o loop de um jogador
            let nomeJogador1 = "Jogador1"
            let nomeJogador2 = "Jogador2"
            -- Supondo que você tenha um modo de jogo para um jogador ou dois jogadores,
            -- você pode adaptar para chamar o loop de jogo adequado.
            -- Aqui, assumimos dois jogadores para o exemplo:
            let palavras = [palavra, palavra, palavra]  -- Ajuste conforme necessário
            loopJogoDoisJogadores conn (T.pack nomeJogador1) (T.pack nomeJogador2) palavras oculta oculta erros erros pontos pontos tentadas tentadas

        else do
        -- Seleção dos jogadores
        putStrLn "Digite o nome do jogador 1: "
        nomeJogador1 <- T.pack <$> getLine

        criaUsuario conn nomeJogador1

        putStrLn "Digite o nome do jogador 2: "
        nomeJogador2 <- T.pack <$> getLine

        criaUsuario conn nomeJogador2

        -- Escolha do nível de dificuldade
        putStrLn "Escolha o nível de dificuldade (fácil, médio, difícil):"
        dificuldade <- getLine
        let dificuldadeLower = map toLower dificuldade

        -- Seleção das palavras com base na dificuldade
        palavras <- selecionarPalavrasPorDificuldade conn dificuldadeLower
        when (length palavras < 6) $ do
            putStrLn "Não há palavras suficientes para este nível de dificuldade. Tente novamente."
            main
            return ()  -- Para evitar o processamento adicional se não houver palavras suficientes

        -- Seleção do tema
        temas <- listarTemas conn
        putStrLn "Escolha um tema:"
        mapM_ (putStrLn . (\(id, nome) -> show id ++ ": " ++ T.unpack nome)) temas
        putStr "Digite o número do tema: "
        temaEscolhido <- readLn

        let temaIndex = temaEscolhido
        if temaIndex <= 0 || temaIndex > length temas
            then do
                putStrLn "Tema inválido. Tente novamente."
                main
            else do
                let (temaId, _) = temas !! (temaIndex - 1)
                palavras <- selecionarPalavrasPorTema conn temaId
                if length palavras < 6
                    then do
                        putStrLn "Não há palavras suficientes para este tema. Tente novamente."
                        main
                    else do
                        -- Escolhe 3 palavras aleatórias para cada jogador
                        palavrasAleatorias <- escolherPalavrasAleatorias palavras 6
                        let (palavra1:palavrasRestantes) = palavrasAleatorias
                        let (palavra2:palavra3:_) = palavrasRestantes

                        -- Inicializa a palavra oculta para ambos os jogadores
                        let oculta1 = inicializarOculta palavra1
                        let oculta2 = inicializarOculta palavra2

                        -- Inicia o loop do jogo
                        loopJogoDoisJogadores conn nomeJogador1 nomeJogador2 [palavra1, palavra2, palavra3] oculta1 oculta2 0 0 0 0 [] []

    close conn


