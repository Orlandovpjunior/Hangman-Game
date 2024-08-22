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

-- Função principal que começa o jogo
main :: IO ()
main = do
    putStrLn "Bem-vindo ao jogo da Forca!"
    putStr "Digite a palavra para o jogo: "
    palavra <- getLine
    let palavraLimpa = filter (/= ' ') (map toLower palavra) -- Remove espaços e converte para minúsculas
    when (null palavraLimpa) $ do
        putStrLn "A palavra não pode ser vazia. Tente novamente."
        main
    jogar palavraLimpa
