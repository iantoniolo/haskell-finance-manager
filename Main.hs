-- Projeto da disciplina de Programacao Funcional
-- Sistema de Controle Financeiro Pessoal
--
-- Compilar: ghc Main.hs -o controle && ./controle
-- Executar: runghc Main.hs

module Main where

import Data.Char   (isDigit, isSpace, toLower)
import Data.List   (nub, sortBy)
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
-- 2. MONOID
-- Resumo = mini-relatorio.
data Resumo = Resumo
    { totalReceitasResumo :: Double
    , totalDespesasResumo :: Double
    , quantidadeReceitasResumo   :: Int
    , quantidadeDespesasResumo   :: Int
    } deriving (Show, Eq)

-- Combinação resumos
instance Semigroup Resumo where
    r1 <> r2 = Resumo
        { totalReceitasResumo = totalReceitasResumo r1 + totalReceitasResumo r2
        , totalDespesasResumo = totalDespesasResumo r1 + totalDespesasResumo r2
        , quantidadeReceitasResumo   = quantidadeReceitasResumo   r1 + quantidadeReceitasResumo   r2
        , quantidadeDespesasResumo   = quantidadeDespesasResumo   r1 + quantidadeDespesasResumo   r2
        }

instance Monoid Resumo where
    mempty = Resumo 0 0 0 0

transacaoParaResumo :: Transacao -> Resumo
transacaoParaResumo t  =
    case tipo t of
        Receita -> Resumo (valor t) 0          1 0
        Despesa -> Resumo 0         (valor t)  0 1

-- map + foldr para concatenar
gerarResumo :: [Transacao] -> Resumo
gerarResumo ts = foldr (<>) mempty (map transacaoParaResumo ts)

saldoSummary :: Resumo -> Double
saldoSummary r  = totalReceitasResumo r - totalDespesasResumo r


-- 3. INSTANCIA DE FUNCTOR
newtype Relatorio a = Relatorio { itemsRelatorio :: [a] }
    deriving (Show)

instance Functor Relatorio where
    fmap f (Relatorio xs) = Relatorio (map f xs)


-- 4. FUNCOES PURAS
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

minusculas :: String -> String
minusculas texto = map toLower texto

buscarCategoria :: String -> [Transacao] -> [Transacao]
buscarCategoria cat ts =
    [ t | t <- ts, minusculas (categoria t) == minusculas cat ]

-- remove as categorias repetidas
categoriasDespesas :: [Transacao] -> [String]
categoriasDespesas ts = nub [ categoria t | t <- ts, ehDespesa t ]

totalCategoria :: String -> [Transacao] -> Double
totalCategoria cat ts =
    somaValores [ t | t <- ts
                    , ehDespesa t
                    , minusculas (categoria t) == minusculas cat ]

-- Functor: Relatorio String -> Relatorio (String, Double)
gastosPorCategoria :: [Transacao] -> Relatorio (String, Double)
gastosPorCategoria ts =
    fmap (\cat -> (cat, totalCategoria cat ts))
         (Relatorio (categoriasDespesas ts))

ordenarPorValor :: [(String, Double)] -> [(String, Double)]
ordenarPorValor pares = sortBy (\(_, a) (_, b) -> compare b a) pares

-- recursao para separar dia, mes ano
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
anoBissexto ano = (ano `mod` 4 == 0 && ano `mod` 100 /= 0) || ano `mod` 400 == 0

diasNoMes :: Int -> Int -> Int
diasNoMes mes ano
    | mes == 2 && anoBissexto ano = 29
    | mes == 2                 = 28
    | mes `elem` [4, 6, 9, 11] = 30
    | otherwise                = 31

-- pattern matching (dd/mm/aaaa)
dataValida :: String -> Bool
dataValida texto =
    case separarData texto of
        [d, m, a] -> numeroEntre 2 1 31 d
                        && numeroEntre 2 1 12 m
                        && numeroEntre 4 1900 2100 a
                        && read d <= diasNoMes (read m) (read a)
        _         -> False


-- 5. FORMATACAO (funcoes que montam Strings)
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


-- 6. ENTRADA E SAIDA (IO)
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

lerData :: String -> IO String
lerData pergunta = do
    entrada <- lerLinha pergunta
    let texto = aparar entrada
    if dataValida texto
        then return texto
        else do
            putStrLn "  ! Data invalida. Use o formato dd/mm/aaaa (ex.: 05/06/2026)."
            lerData pergunta

-- ---------- Cadastro ----------

-- devolve uma lista NOVA com a transacao no fim
cadastrar :: TipoTransacao -> [Transacao] -> IO [Transacao]
cadastrar tp ts = do
    putStrLn ("\n--- Cadastro de " ++ show tp ++ " ---")
    d  <- lerTexto "Descricao         : "
    v  <- lerValor "Valor (R$)        : "
    c  <- lerTexto "Categoria         : "
    dt <- lerData "Data (dd/mm/aaaa) : "
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
    putStrLn linha
    exibirTransacoes resto

exibirMaiorGasto :: [Transacao] -> IO ()
exibirMaiorGasto ts =
    case maiorGasto ts of
        Nothing -> putStrLn "Nenhuma despesa cadastrada."
        Just t  -> do
            putStrLn ("Descricao: " ++ descricao t)
            putStrLn ("Categoria: " ++ categoria t)
            putStrLn ("Valor....: " ++ formatCurrency (valor t))

imprimirCategorias :: [(String, Double)] -> IO ()
imprimirCategorias []               = return ()
imprimirCategorias ((c, v) : resto) = do
    putStrLn (c ++ ": " ++ formatCurrency v)
    imprimirCategorias resto

exibirGastosCategoria :: [Transacao] -> IO ()
exibirGastosCategoria ts =
    let lista = ordenarPorValor (itemsRelatorio (gastosPorCategoria ts))
    in if null lista
          then putStrLn "Nenhuma despesa cadastrada."
          else imprimirCategorias lista

buscarCategoriaIO :: [Transacao] -> IO ()
buscarCategoriaIO ts = do
    cat <- lerTexto "Categoria que deseja buscar: "
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

-- Resumo (Monoid)
exibirRelatorio :: [Transacao] -> IO ()
exibirRelatorio ts = do
    let r = gerarResumo ts
    putStrLn "========== RELATORIO COMPLETO =========="
    putStrLn ""
    putStrLn ("Quantidade de receitas : " ++ show (quantidadeReceitasResumo r))
    putStrLn ("Quantidade de despesas : " ++ show (quantidadeDespesasResumo r))
    putStrLn ("Total de receitas      : " ++ formatCurrency (totalReceitasResumo r))
    putStrLn ("Total de despesas      : " ++ formatCurrency (totalDespesasResumo r))
    putStrLn ("Saldo atual            : " ++ formatCurrency (saldoSummary r))
    putStrLn ("Media de gastos        : " ++ formatCurrency (mediaDespesas ts))
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


-- 7. MENU E LOOP PRINCIPAL
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
            putStrLn ("Media de gastos: " ++ formatCurrency (mediaDespesas ts))
            putStrLn ("Quantidade        : " ++ show (contar (despesas ts)))
            loop ts

        "9" -> do
            buscarCategoriaIO ts
            loop ts

        "10" -> do
            exibirRelatorio ts
            loop ts

        "0" -> putStrLn "Encerrando o sistema. Ate logo!"

        _   -> do
            putStrLn "Opcao invalida! Digite um numero de 0 a 10."
            loop ts


-- 8. DADOS DE EXEMPLO
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