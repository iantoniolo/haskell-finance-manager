-- Projeto da disciplina de Programacao Funcional
-- Sistema de Controle Financeiro Pessoal
--
-- Compilar: ghc Main.hs -o controle && ./controle
-- Executar: runghc Main.hs

module Main where

import Data.Char   (isSpace, toLower)
import Data.List   (nub, sortBy)
import System.IO   (hFlush, hSetEncoding, stdin, stdout, utf8)
import Text.Printf (printf)


-- ============================================================
-- 1. TIPOS PERSONALIZADOS
-- ============================================================

data TipoTransacao
    = Receita
    | Despesa
    deriving (Show, Eq)

-- Record syntax: cada campo vira uma funcao de acesso (ex.: valor :: Transacao -> Double)
data Transacao = Transacao
    { descricao     :: String
    , valor         :: Double
    , categoria     :: String
    , dataTransacao :: String
    , tipo          :: TipoTransacao
    } deriving (Show, Eq)


-- ============================================================
-- 2. MONOID
-- Resumo = mini-relatorio.
-- ============================================================

data Resumo = Resumo
    { totalRec :: Double
    , totalDes :: Double
    , qtdRec   :: Int
    , qtdDes   :: Int
    } deriving (Show, Eq)

-- Combinação resumos
instance Semigroup Resumo where
    r1 <> r2 = Resumo
        { totalRec = totalRec r1 + totalRec r2
        , totalDes = totalDes r1 + totalDes r2
        , qtdRec   = qtdRec   r1 + qtdRec   r2
        , qtdDes   = qtdDes   r1 + qtdDes   r2
        }

instance Monoid Resumo where
    mempty = Resumo 0 0 0 0

transacaoParaResumo :: Transacao -> Resumo
transacaoParaResumo t =
    case tipo t of
        Receita -> Resumo (valor t) 0          1 0
        Despesa -> Resumo 0         (valor t)  0 1

-- map + foldr para concatenar
gerarResumo :: [Transacao] -> Resumo
gerarResumo ts = foldr (<>) mempty (map transacaoParaResumo ts)

saldoDoResumo :: Resumo -> Double
saldoDoResumo r = totalRec r - totalDes r


-- ============================================================
-- 3. INSTANCIA DE FUNCTOR
-- ============================================================

newtype Relatorio a = Relatorio { itens :: [a] }
    deriving (Show)

instance Functor Relatorio where
    fmap f (Relatorio xs) = Relatorio (map f xs)


-- ============================================================
-- 4. FUNCOES PURAS
-- ============================================================

ehReceita :: Transacao -> Bool
ehReceita t = tipo t == Receita

ehDespesa :: Transacao -> Bool
ehDespesa t = tipo t == Despesa

-- filter
receitas :: [Transacao] -> [Transacao]
receitas ts = filter ehReceita ts

-- list comprehension
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
categoriasDeDespesa :: [Transacao] -> [String]
categoriasDeDespesa ts = nub [ categoria t | t <- ts, ehDespesa t ]

totalDaCategoria :: String -> [Transacao] -> Double
totalDaCategoria cat ts =
    somaValores [ t | t <- ts
                    , ehDespesa t
                    , minusculas (categoria t) == minusculas cat ]

-- Functor: Relatorio String -> Relatorio (String, Double)
gastosPorCategoria :: [Transacao] -> Relatorio (String, Double)
gastosPorCategoria ts =
    fmap (\cat -> (cat, totalDaCategoria cat ts))
         (Relatorio (categoriasDeDespesa ts))

ordenarPorValor :: [(String, Double)] -> [(String, Double)]
ordenarPorValor pares = sortBy (\(_, a) (_, b) -> compare b a) pares


-- ============================================================
-- 5. FORMATACAO (funcoes que montam Strings)
-- ============================================================

formatarValor :: Double -> String
formatarValor v = printf "R$ %.2f" v

linha :: String
linha = replicate 45 '-'

textoTransacao :: Transacao -> String
textoTransacao t =
    unlines
        [ show (tipo t)
        , "Descricao : " ++ descricao t
        , "Valor     : " ++ formatarValor (valor t)
        , "Categoria : " ++ categoria t
        , "Data      : " ++ dataTransacao t
        ]


-- ============================================================
-- 6. ENTRADA E SAIDA (IO)
-- ============================================================

lerLinha :: String -> IO String
lerLinha pergunta = do
    putStr pergunta
    hFlush stdout
    getLine

-- remove espacos das pontas
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
    d  <- lerTexto "Descricao         : "
    v  <- lerValor "Valor (R$)        : "
    c  <- lerTexto "Categoria         : "
    dt <- lerTexto "Data (dd/mm/aaaa) : "
    let nova = Transacao
            { descricao     = d
            , valor         = v
            , categoria     = c
            , dataTransacao = dt
            , tipo          = tp
            }
    putStrLn (">> " ++ show tp ++ " cadastrada com sucesso!")
    return (ts ++ [nova])

adicionarReceita :: [Transacao] -> IO [Transacao]
adicionarReceita ts = cadastrar Receita ts

adicionarDespesa :: [Transacao] -> IO [Transacao]
adicionarDespesa ts = cadastrar Despesa ts


-- ---------- Relatorios na tela ----------

-- impressao recursiva
listarTransacoes :: [Transacao] -> IO ()
listarTransacoes []          = putStrLn "Nenhuma transacao cadastrada ainda."
listarTransacoes [t]         = putStr (textoTransacao t)
listarTransacoes (t : resto) = do
    putStr (textoTransacao t)
    putStrLn ""
    putStrLn linha
    putStrLn ""
    listarTransacoes resto

mostrarMaiorGasto :: [Transacao] -> IO ()
mostrarMaiorGasto ts =
    case maiorGasto ts of
        Nothing -> putStrLn "Nenhuma despesa cadastrada."
        Just t  -> do
            putStrLn ("Descricao : " ++ descricao t)
            putStrLn ("Categoria : " ++ categoria t)
            putStrLn ("Valor     : " ++ formatarValor (valor t))

imprimirCategorias :: [(String, Double)] -> IO ()
imprimirCategorias []               = return ()
imprimirCategorias ((c, v) : resto) = do
    putStrLn (c ++ ": " ++ formatarValor v)
    imprimirCategorias resto

mostrarGastosPorCategoria :: [Transacao] -> IO ()
mostrarGastosPorCategoria ts =
    let lista = ordenarPorValor (itens (gastosPorCategoria ts))
    in if null lista
          then putStrLn "Nenhuma despesa cadastrada."
          else imprimirCategorias lista

menuBuscarCategoria :: [Transacao] -> IO ()
menuBuscarCategoria ts = do
    cat <- lerTexto "Categoria que deseja buscar: "
    let achadas = buscarCategoria cat ts
    putStrLn ""
    if null achadas
        then putStrLn ("Nenhuma transacao encontrada na categoria \"" ++ cat ++ "\".")
        else do
            putStrLn ("Transacoes da categoria \"" ++ cat ++ "\":")
            putStrLn linha
            listarTransacoes achadas
            putStrLn linha
            putStrLn ("Total movimentado: " ++ formatarValor (somaValores achadas))

-- Resumo (Monoid)
relatorioCompleto :: [Transacao] -> IO ()
relatorioCompleto ts = do
    let r = gerarResumo ts
    putStrLn "========== RELATORIO COMPLETO =========="
    putStrLn ""
    putStrLn ("Quantidade de receitas : " ++ show (qtdRec r))
    putStrLn ("Quantidade de despesas : " ++ show (qtdDes r))
    putStrLn ("Total de receitas      : " ++ formatarValor (totalRec r))
    putStrLn ("Total de despesas      : " ++ formatarValor (totalDes r))
    putStrLn ("Saldo atual            : " ++ formatarValor (saldoDoResumo r))
    putStrLn ("Media de gastos        : " ++ formatarValor (mediaDespesas ts))
    putStrLn ""
    putStrLn "--- Maior gasto ---"
    mostrarMaiorGasto ts
    putStrLn ""
    putStrLn "--- Gastos por categoria ---"
    mostrarGastosPorCategoria ts
    putStrLn ""
    putStrLn "--- Todas as transacoes ---"
    listarTransacoes ts
    putStrLn ""
    putStrLn "========================================"


-- ============================================================
-- 7. MENU E LOOP PRINCIPAL
-- ============================================================

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
            listarTransacoes ts
            loop ts

        "4" -> do
            putStrLn ("Saldo atual: " ++ formatarValor (calcularSaldo ts))
            loop ts

        "5" -> do
            putStrLn ("Total de receitas : " ++ formatarValor (totalReceitas ts))
            putStrLn ("Quantidade        : " ++ show (contar (receitas ts)))
            loop ts

        "6" -> do
            putStrLn ("Total de despesas : " ++ formatarValor (totalDespesas ts))
            putStrLn ("Quantidade        : " ++ show (contar (despesas ts)))
            loop ts

        "7" -> do
            putStrLn "--- MAIOR GASTO ---"
            mostrarMaiorGasto ts
            loop ts

        "8" -> do
            putStrLn ("Media de gastos: " ++ formatarValor (mediaDespesas ts))
            loop ts

        "9" -> do
            menuBuscarCategoria ts
            loop ts

        "10" -> do
            relatorioCompleto ts
            loop ts

        "0" -> putStrLn "Encerrando o sistema. Ate logo!"

        _   -> do
            putStrLn "Opcao invalida! Digite um numero de 0 a 10."
            loop ts


-- ============================================================
-- 8. DADOS DE EXEMPLO
-- ============================================================

transacoesExemplo :: [Transacao]
transacoesExemplo =
    [ Transacao "Salario"        3500.00 "Trabalho"     "05/06/2026" Receita
    , Transacao "Freelance"       800.00 "Trabalho"     "12/06/2026" Receita
    , Transacao "Mercado"         250.00 "Alimentacao"  "06/06/2026" Despesa
    , Transacao "Restaurante"     100.00 "Alimentacao"  "09/06/2026" Despesa
    , Transacao "Combustivel"     240.00 "Transporte"   "07/06/2026" Despesa
    , Transacao "Aluguel"        1200.00 "Moradia"      "10/06/2026" Despesa
    ]


-- ============================================================
-- 7. PONTO DE ENTRADA
-- ============================================================

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin  utf8
    putStrLn "Bem-vindo(a) ao Controle Financeiro Pessoal!"
    putStrLn "(o sistema ja inicia com algumas transacoes de exemplo)"
    loop transacoesExemplo
