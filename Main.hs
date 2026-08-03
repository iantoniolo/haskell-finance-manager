-- Projeto da disciplina de Programacao Funcional
-- Sistema de Controle Financeiro Pessoal
--
-- Compilar: ghc Main.hs -o controle && ./controle
-- Executar: runghc Main.hs

module Main where

import Data.Char   (isSpace)
import System.IO   (hFlush, hSetEncoding, stdin, stdout, utf8)
import Text.Printf (printf)


-- 1. TIPOS PERSONALIZADOS
data TipoTransacao
    = Receita
    | Despesa
    deriving (Show, Eq)

-- Record syntax: cada campo vira uma funcao de acesso (ex.: valor :: Transacao -> Double)
data Transacao = Transacao
    { descricao     :: String
    , valorT :: Double
    , categoria     :: String
    , dataDaTransacaoBR :: String
    , tipoTransacao :: TipoTransacao
    } deriving (Show, Eq)




valor :: Transacao -> Double
valor = valorT

tipo :: Transacao -> TipoTransacao
tipo = tipoTransacao
-- 2. FUNCOES
ehReceita :: Transacao -> Bool
ehReceita t = tipo t == Receita

ehDespesa :: Transacao -> Bool
ehDespesa t = tipo t == Despesa

receitas :: [Transacao] -> [Transacao]
receitas ts = filter ehReceita ts

despesas :: [Transacao] -> [Transacao]
despesas ts = [ t | t <- ts, ehDespesa t ]

-- recursao: soma da lista vazia e 0; senao, primeiro + soma do resto
somaValores :: [Transacao] -> Double
somaValores []           = 0
somaValores (t : resto)  = valor t + somaValores resto

-- recursao
contar :: [a] -> Int
contar []           = 0
contar (_ : resto)  = 1 + contar resto

totalReceitas :: [Transacao] -> Double
totalReceitas ts = foldr (+) 0 (map valor (receitas ts))

totalDespesas :: [Transacao] -> Double
totalDespesas ts = somaValores (despesas ts)

calcularSaldo :: [Transacao] -> Double
calcularSaldo ts = totalReceitas ts - totalDespesas ts

-- Maybe
maiorGasto :: [Transacao] -> Maybe Transacao
maiorGasto ts = maiorDaLista (despesas ts)
  where
    maiorDaLista []            = Nothing
    maiorDaLista [t]           = Just t
    maiorDaLista (t : resto)   =
        case maiorDaLista resto of
            Just m | valor m > valor t -> Just m
            _                          -> Just t


mediaDespesas :: [Transacao] -> Double
mediaDespesas ts
    | contar ds == 0 = 0
    | otherwise      = somaValores ds / fromIntegral (contar ds)
  where
    ds = despesas ts


-- 3. FORMATACAO (funcoes que montam Strings)
formatCurrency :: Double -> String
formatCurrency v  = printf "R$ %.2f" v

linha :: String
linha = replicate 45 '-'

textoTransacao :: Transacao -> String
textoTransacao t  =
    unlines
        [ show (tipo t)
        , "Descricao : " ++ descricao t
        , "Valor     : " ++ formatCurrency (valor t)
        , "Categoria : " ++ categoria t
        , "Data      : " ++ dataDaTransacaoBR t
        ]


-- 4. ENTRADA E SAIDA (IO)
lerLinha :: String -> IO String
lerLinha pergunta = do
    putStr pergunta
    hFlush stdout
    getLine

aparar :: String -> String
aparar texto = tirar (tirar texto)
  where
    tirar = reverse . dropWhile isSpace

-- verificação null
lerTexto :: String -> IO String
lerTexto pergunta = do
    entrada <- lerLinha pergunta
    let texto = aparar entrada
    if null texto
        then do
            putStrLn "  ! Campo obrigatorio, tente novamente."
            lerTexto pergunta
        else return texto

-- read
lerValor :: String -> IO Double
lerValor pergunta = do
    entrada <- lerLinha pergunta
    case reads (trocarVirgula (aparar entrada)) :: [(Double, String)] of
        [(v, "")] | v > 0 -> return v
        _ -> do
            putStrLn "  ! Valor invalido. Digite um numero maior que zero (ex.: 1250.50)."
            lerValor pergunta
  where
    trocarVirgula = map (\c -> if c == ',' then '.' else c)


-- ---------- Cadastro ----------

-- devolve uma lista NOVA com a transacao no fim
cadastrar :: TipoTransacao -> [Transacao] -> IO [Transacao]
cadastrar tp ts = do
    putStrLn ("\n--- Cadastro de " ++ show tp ++ " ---")
    d  <- lerTexto "Descricao........: "
    v  <- lerValor "Valor (R$).......: "
    c  <- lerTexto "Categoria........: "
    dt <- lerTexto "Data (dd/mm/aaaa): "
    let nova = Transacao
            { descricao     = d
            , valorT = v
            , categoria     = c
            , dataDaTransacaoBR = dt
            , tipoTransacao = tp
            }
    putStrLn (">> " ++ show tp ++ " cadastrada com sucesso!")
    return (ts ++ [nova])

adicionarReceita :: [Transacao] -> IO [Transacao]
adicionarReceita ts = cadastrar Receita ts

adicionarDespesa :: [Transacao] -> IO [Transacao]
adicionarDespesa ts = cadastrar Despesa ts


-- ---------- Relatorios na tela ----------

-- impressao recursiva
exibirTransacoes :: [Transacao] -> IO ()
exibirTransacoes []          = putStrLn "Nenhuma transacao cadastrada ainda."
exibirTransacoes [t]         = putStr (textoTransacao t)
exibirTransacoes (t : resto) = do
    putStr (textoTransacao t)
    putStrLn ""
    putStrLn linha
    putStrLn ""
    exibirTransacoes resto

exibirMaiorGasto :: [Transacao] -> IO ()
exibirMaiorGasto ts =
    case maiorGasto ts of
        Nothing -> putStrLn "Nenhuma despesa cadastrada."
        Just t  -> do
            putStrLn ("Descricao : " ++ descricao t)
            putStrLn ("Categoria : " ++ categoria t)
            putStrLn ("Valor     : " ++ formatCurrency (valor t))


-- 5. MENU E LOOP PRINCIPAL
menu :: IO ()
menu = putStr (unlines
    [ ""
    , "======== CONTROLE FINANCEIRO PESSOAL ========"
    , " 1 - Cadastrar receita"
    , " 2 - Cadastrar despesa"
    , " 3 - Listar todas as transacoes"
    , " 4 - Mostrar saldo atual"
    , " 5 - Mostrar total de receitas"
    , " 6 - Mostrar total de despesas"
    , " 7 - Mostrar maior gasto"
    , " 8 - Mostrar media de gastos"
    , " 0 - Sair"
    , "============================================="
    ])

-- A lista e o argumento da funcao
-- Recursao com a lista nova a cada chamada
loop :: [Transacao] -> IO ()
loop ts = do
    menu
    opcao <- lerLinha "Escolha uma opcao: "
    putStrLn ""
    case aparar opcao of

        "1" -> do
            novaLista <- adicionarReceita ts
            loop novaLista

        "2" -> do
            novaLista <- adicionarDespesa ts
            loop novaLista

        "3" -> do
            putStrLn "--- TODAS AS TRANSACOES ---"
            exibirTransacoes ts
            loop ts

        "4" -> do
            putStrLn ("Saldo atual: " ++ formatCurrency (calcularSaldo ts))
            loop ts

        "5" -> do
            putStrLn ("Total de receitas : " ++ formatCurrency (totalReceitas ts))
            putStrLn ("Quantidade        : " ++ show (contar (receitas ts)))
            loop ts

        "6" -> do
            putStrLn ("Total de despesas : " ++ formatCurrency (totalDespesas ts))
            putStrLn ("Quantidade        : " ++ show (contar (despesas ts)))
            loop ts

        "7" -> do
            putStrLn "--- MAIOR GASTO ---"
            exibirMaiorGasto ts
            loop ts

        "8" -> do
            putStrLn ("Media de gastos : " ++ formatCurrency (mediaDespesas ts))
            putStrLn ("Quantidade      : " ++ show (contar (despesas ts)))
            loop ts

        "0" -> putStrLn "Encerrando o sistema. Ate logo!"

        _   -> do
            putStrLn "Opcao invalida! Digite um numero de 0 a 8."
            loop ts


-- 6. DADOS DE EXEMPLO (troque por [] em main para comecar vazio)
transacoesIniciais :: [Transacao]
transacoesIniciais =
    [ Transacao "Salario"        3500.00 "Trabalho"     "05/06/2026" Receita
    , Transacao "Freelance"       800.00 "Trabalho"     "12/06/2026" Receita
    , Transacao "Mercado"         250.00 "Alimentacao"  "06/06/2026" Despesa
    , Transacao "Restaurante"     100.00 "Alimentacao"  "09/06/2026" Despesa
    , Transacao "Combustivel"     240.00 "Transporte"   "07/06/2026" Despesa
    , Transacao "Aluguel"        1200.00 "Moradia"      "10/06/2026" Despesa
    ]


-- 7. PONTO DE ENTRADA
main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin  utf8
    putStrLn "Bem-vindo(a) ao Controle Financeiro Pessoal!"
    putStrLn "(o sistema ja inicia com algumas transacoes de exemplo)"
    loop transacoesIniciais
