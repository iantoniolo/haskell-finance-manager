-- Projeto da disciplina de Programacao Funcional
-- Sistema de Controle Financeiro Pessoal
--
-- Compilar: ghc Main.hs -o controle && ./controle
-- Executar: runghc Main.hs

module Main where

import Data.Char   (isDigit, isSpace, toLower)
import Data.List   (intercalate, nub, sortBy)
import System.IO   (hFlush, hSetEncoding, stdin, stdout, utf8)
import Text.Printf (printf)


-- 1. TIPOS

type Categoria = String
type DataBR    = String
type Valor     = Double

data TipoTransacao
    = Receita
    | Despesa
    deriving (Show, Eq)

data Transacao = Transacao
    { descricao :: String
    , valorT :: Valor
    , categoria :: Categoria
    , dataDaTransacaoBR :: DataBR
    , tipoTransacao :: TipoTransacao
    } deriving (Show, Eq)

valor :: Transacao -> Valor
valor = valorT

tipo :: Transacao -> TipoTransacao
tipo = tipoTransacao

-- 2. MONOID

data Resumo = Resumo
    { totalReceitasResumo :: Valor
    , totalDespesasResumo :: Valor
    , quantidadeReceitasResumo :: Int
    , quantidadeDespesasResumo :: Int
    } deriving (Show, Eq)

-- comabina 2 resumos somando os campos
instance Semigroup Resumo where
    resumoLeft <> resumoRight = Resumo
        { totalReceitasResumo = totalReceitasResumo resumoLeft + totalReceitasResumo resumoRight
        , totalDespesasResumo = totalDespesasResumo resumoLeft + totalDespesasResumo resumoRight
        , quantidadeReceitasResumo = quantidadeReceitasResumo resumoLeft + quantidadeReceitasResumo resumoRight
        , quantidadeDespesasResumo = quantidadeDespesasResumo resumoLeft + quantidadeDespesasResumo resumoRight
        }

instance Monoid Resumo where
    mempty = Resumo 0 0 0 0

transacaoParaResumo :: Transacao -> Resumo
transacaoParaResumo transacaoAtual  =
    case tipo transacaoAtual of
        Receita -> Resumo (valor transacaoAtual) 0 1 0
        Despesa -> Resumo 0 (valor transacaoAtual) 0 1

gerarResumo :: [Transacao] -> Resumo
gerarResumo = foldMap transacaoParaResumo

saldoSummary :: Resumo -> Valor
saldoSummary resumoAtual  =
    totalReceitasResumo resumoAtual - totalDespesasResumo resumoAtual

-- nao precissa percorrer a lista duas vezes pra calcular a media
mediaResumo :: Resumo -> Valor
mediaResumo resumoAtual
    | quantidadeDespesasResumo resumoAtual == 0 = 0
    | otherwise = totalDespesasResumo resumoAtual / fromIntegral (quantidadeDespesasResumo resumoAtual)


-- 3. FUNCTOR
newtype Relatorio a = Relatorio { itemsRelatorio :: [a] }
    deriving (Show)

instance Functor Relatorio where
    fmap transformarItem (Relatorio itemsList) =
        Relatorio (map transformarItem itemsList)

-- 4. FUNCOES PURAS

ehReceita :: Transacao -> Bool
ehReceita t = tipo t == Receita

ehDespesa :: Transacao -> Bool
ehDespesa t = tipo t == Despesa

receitas :: [Transacao] -> [Transacao]
receitas = filter ehReceita

despesas :: [Transacao] -> [Transacao]
despesas ts = [ t | t <- ts, ehDespesa t ]

somaValores :: [Transacao] -> Valor
somaValores []          = 0
somaValores (t : resto) = valor t + somaValores resto

contar :: [a] -> Int
contar [] = 0
contar (_ : restanteDaLista) = 1 + contar restanteDaLista

totalReceitas :: [Transacao] -> Valor
totalReceitas = foldr (\t acc -> valor t + acc) 0 . receitas

totalDespesas :: [Transacao] -> Valor
totalDespesas = somaValores . despesas

calcularSaldo :: [Transacao] -> Valor
calcularSaldo ts = totalReceitas ts - totalDespesas ts

mediaDespesas :: [Transacao] -> Valor
mediaDespesas = mediaResumo . gerarResumo

maiorGasto :: [Transacao] -> Maybe Transacao
maiorGasto ts = maiorDaLista (despesas ts)
  where
    maiorDaLista []          = Nothing
    maiorDaLista (t : resto) =
        case maiorDaLista resto of
            Just m | valor m > valor t -> Just m
            _                          -> Just t

minusculas :: String -> String
minusculas = map toLower

mesmaCategoria :: Categoria -> Transacao -> Bool
mesmaCategoria cat t =
    minusculas (categoria t) == minusculas cat

buscarCategoria :: Categoria -> [Transacao] -> [Transacao]
buscarCategoria cat ts = [ t | t <- ts, mesmaCategoria cat t ]

categoriasDespesas :: [Transacao] -> [Categoria]
categoriasDespesas ts = nub [ categoria t | t <- ts, ehDespesa t ]

totalCategoria :: Categoria -> [Transacao] -> Valor
totalCategoria cat ts =
    somaValores [ t | t <- ts, ehDespesa t, mesmaCategoria cat t ]

gastosPorCategoria :: [Transacao] -> Relatorio (Categoria, Valor)
gastosPorCategoria ts =
    fmap (\cat -> (cat, totalCategoria cat ts))
         (Relatorio (categoriasDespesas ts))

ordenarPorValor :: [(Categoria, Valor)] -> [(Categoria, Valor)]
ordenarPorValor =
    sortBy (\(_, valorA) (_, valorB) -> compare valorB valorA)

-- 5. VALIDACOES PURAS
separarData :: String -> [String]
separarData texto =
    case break (== '/') texto of
        (parte, [])        -> [parte]
        (parte, _ : resto) -> parte : separarData resto

numeroEntre :: Int -> Int -> Int -> String -> Bool
numeroEntre tamanho minimo maximo texto =
    contar texto == tamanho
        && all isDigit texto
        && numero >= minimo
        && numero <= maximo
  where
    numero = read texto :: Int

anoBissexto :: Int -> Bool
anoBissexto ano =
    (ano `mod` 4 == 0 && ano `mod` 100 /= 0) || ano `mod` 400 == 0

diasNoMes :: Int -> Int -> Int
diasNoMes mes ano
    | mes == 2 && anoBissexto ano = 29
    | mes == 2 = 28
    | mes `elem` [4, 6, 9, 11] = 30
    | otherwise = 31

dataValida :: DataBR -> Bool
dataValida texto =
    case separarData texto of
        [d, m, a] -> numeroEntre 2 1 31 d
                        && numeroEntre 2 1 12 m
                        && numeroEntre 4 1900 2100 a
                        && read d <= diasNoMes (read m) (read a)
        _         -> False

naoVazio :: String -> Maybe String
naoVazio ""    = Nothing
naoVazio texto = Just texto

valorPositivo :: String -> Maybe Valor
valorPositivo texto =
    case reads (map trocaVirgula texto) :: [(Valor, String)] of
        [(v, "")] | v > 0 -> Just v
        _                 -> Nothing
  where
    trocaVirgula c = if c == ',' then '.' else c

dataBR :: String -> Maybe DataBR
dataBR texto
    | dataValida texto = Just texto
    | otherwise        = Nothing


-- 6. FORMATACAO (funcoes que so montam Strings)

formatCurrency :: Valor -> String
formatCurrency valorAtual  = printf "R$ %.2f" valorAtual

linha :: String
linha = replicate 45 '-'

textoTransacao :: Transacao -> String
textoTransacao transacaoAtual  =
    unlines
        [ show (tipoTransacao transacaoAtual)
        , "Descricao : " ++ descricao transacaoAtual
        , "Valor     : " ++ formatCurrency (valorT transacaoAtual)
        , "Categoria : " ++ categoria transacaoAtual
        , "Data      : " ++ dataDaTransacaoBR transacaoAtual
        ]

-- 7. ENTRADA E SAIDA (IO)

lerLinha :: String -> IO String
lerLinha pergunta = do
    putStr pergunta
    hFlush stdout
    getLine

aparar :: String -> String
aparar = tirar . tirar
  where
    tirar = reverse . dropWhile isSpace

lerCampo :: String -> (String -> Maybe a) -> String -> IO a
lerCampo pergunta valida erro = do
    entrada <- lerLinha pergunta
    case valida (aparar entrada) of
        Just x  -> return x
        Nothing -> do
            putStrLn ("  ! " ++ erro)
            lerCampo pergunta valida erro


cadastrar :: TipoTransacao -> [Transacao] -> IO [Transacao]
cadastrar tp ts = do
    putStrLn ("\n--- Cadastro de " ++ show tp ++ " ---")
    d  <- lerCampo "Descricao         : " naoVazio      "Campo obrigatorio."
    v  <- lerCampo "Valor (R$)        : " valorPositivo "Valor invalido. Digite um numero maior que zero (ex.: 1250.50)."
    c  <- lerCampo "Categoria         : " naoVazio      "Campo obrigatorio."
    dt <- lerCampo "Data (dd/mm/aaaa) : " dataBR        "Data invalida. Use o formato dd/mm/aaaa (ex.: 05/06/2026)."
    putStrLn (">> " ++ show tp ++ " cadastrada com sucesso!")
    return
        (ts ++
            [ Transacao
                { descricao = d
                , valorT = v
                , categoria = c
                , dataDaTransacaoBR = dt
                , tipoTransacao = tp
                }
            ])

exibirTransacoes :: [Transacao] -> IO ()
exibirTransacoes [] = putStrLn "Nenhuma transacao cadastrada ainda."
exibirTransacoes ts =
    putStr (intercalate (linha ++ "\n") (map textoTransacao ts))

exibirMaiorGasto :: [Transacao] -> IO ()
exibirMaiorGasto ts =
    case maiorGasto ts of
        Nothing -> putStrLn "Nenhuma despesa cadastrada."
        Just t  -> do
            putStrLn ("Descricao: " ++ descricao t)
            putStrLn ("Categoria: " ++ categoria t)
            putStrLn ("Valor....: " ++ formatCurrency (valorT t))

exibirGastosCategoria :: [Transacao] -> IO ()
exibirGastosCategoria ts
    | null lista = putStrLn "Nenhuma despesa cadastrada."
    | otherwise = putStr (unlines [ c ++ ": " ++ formatCurrency v | (c, v) <- lista ])
  where
    lista = ordenarPorValor (itemsRelatorio (gastosPorCategoria ts))

exibirTotal :: String -> Valor -> Int -> IO ()
exibirTotal nomeTotal valorTotal quantidadeTotal = do
    putStrLn ("Total de " ++ nomeTotal ++ " : " ++ formatCurrency valorTotal)
    putStrLn ("Quantidade        : " ++ show quantidadeTotal)

buscarCategoriaIO :: [Transacao] -> IO ()
buscarCategoriaIO ts = do
    cat <- lerCampo "Categoria que deseja buscar: " naoVazio "Campo obrigatorio."
    let achadas = buscarCategoria cat ts
    putStrLn ""
    if null achadas
        then putStrLn ("Nenhuma transacao encontrada na categoria \"" ++ cat ++ "\".")
        else do
            putStrLn ("Transacoes da categoria \"" ++ cat ++ "\":")
            putStrLn linha
            exibirTransacoes achadas
            putStrLn linha
            putStrLn ("Total movimentado: " ++ formatCurrency (somaValores achadas))

exibirRelatorio :: [Transacao] -> IO ()
exibirRelatorio ts = do
    let resumoAtual = gerarResumo ts
    putStrLn "========== RELATORIO COMPLETO =========="
    putStrLn ""
    putStrLn ("Quantidade de receitas : " ++ show (quantidadeReceitasResumo resumoAtual))
    putStrLn ("Quantidade de despesas : " ++ show (quantidadeDespesasResumo resumoAtual))
    putStrLn ("Total de receitas      : " ++ formatCurrency (totalReceitasResumo resumoAtual))
    putStrLn ("Total de despesas      : " ++ formatCurrency (totalDespesasResumo resumoAtual))
    putStrLn ("Saldo atual            : " ++ formatCurrency (saldoSummary resumoAtual))
    putStrLn ("Media de gastos        : " ++ formatCurrency (mediaResumo resumoAtual))
    putStrLn ""
    putStrLn "--- Maior gasto ---"
    exibirMaiorGasto ts
    putStrLn ""
    putStrLn "--- Gastos por categoria ---"
    exibirGastosCategoria ts
    putStrLn ""
    putStrLn "--- Todas as transacoes ---"
    exibirTransacoes ts
    putStrLn ""
    putStrLn "========================================"

-- 8. MENU E LOOP PRINCIPAL

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
    , " 9 - Buscar transacoes por categoria"
    , "10 - Relatorio completo"
    , " 0 - Sair"
    , "============================================="
    ])

consultar :: String -> [Transacao] -> IO ()
consultar op ts =
    case op of
        "3"  -> do putStrLn "--- TODAS AS TRANSACOES ---"
                   exibirTransacoes ts
        "4" -> putStrLn ("Saldo atual: " ++ formatCurrency (calcularSaldo ts))
        "5" -> exibirTotal "receitas" (totalReceitas ts) (contar (receitas ts))
        "6" -> exibirTotal "despesas" (totalDespesas ts) (contar (despesas ts))
        "7"  -> do putStrLn "--- MAIOR GASTO ---"
                   exibirMaiorGasto ts
        "8" -> do
            putStrLn ("Media de gastos   : " ++ formatCurrency (mediaDespesas ts))
            putStrLn ("Quantidade        : " ++ show (contar (despesas ts)))
        "9" -> buscarCategoriaIO ts
        "10" -> exibirRelatorio ts
        _    -> putStrLn "Opcao invalida! Digite um numero de 0 a 10."

loop :: [Transacao] -> IO ()
loop ts = do
    menu
    opcao <- lerLinha "Escolha uma opcao: "
    putStrLn ""
    case aparar opcao of
        "0" -> putStrLn "Encerrando o sistema. Ate logo!"
        "1" -> do novaLista <- cadastrar Receita ts
                  loop novaLista
        "2" -> do novaLista <- cadastrar Despesa ts
                  loop novaLista
        op  -> do consultar op ts
                  loop ts

-- 9. DADOS DE EXEMPLO E PONTO DE ENTRADA

transacoesIniciais :: [Transacao]
transacoesIniciais =
    [ Transacao "Salario"        3500.00 "Trabalho"     "05/06/2026" Receita
    , Transacao "Freelance"       800.00 "Trabalho"     "12/06/2026" Receita
    , Transacao "Mercado"         250.00 "Alimentacao"  "06/06/2026" Despesa
    , Transacao "Restaurante"     100.00 "Alimentacao"  "09/06/2026" Despesa
    , Transacao "Combustivel"     240.00 "Transporte"   "07/06/2026" Despesa
    , Transacao "Aluguel"        1200.00 "Moradia"      "10/06/2026" Despesa
    ]

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin  utf8
    putStrLn "Bem-vindo(a) ao Controle Financeiro Pessoal!"
    putStrLn "(o sistema ja inicia com algumas transacoes de exemplo)"
    loop transacoesIniciais
