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


-- ============================================================
-- 1. TIPOS
-- ============================================================

-- type: apenas apelidos, para a assinatura das funcoes dizer
-- o que cada String/Double significa
type Categoria = String
type DataBR    = String
type Valor     = Double

-- tipo soma: ou e uma coisa, ou e outra
data TipoTransacao
    = Receita
    | Despesa
    deriving (Show, Eq)

-- tipo produto com record syntax:
-- cada campo vira uma funcao de acesso (ex.: valor :: Transacao -> Valor)
data Transacao = Transacao
    { descricao     :: String
    , valor         :: Valor
    , categoria     :: Categoria
    , dataTransacao :: DataBR
    , tipo          :: TipoTransacao
    } deriving (Show, Eq)


-- ============================================================
-- 2. MONOID
-- Resumo = mini-relatorio que sabe se combinar com outro resumo.
-- ============================================================

data Resumo = Resumo
    { totalRec :: Valor
    , totalDes :: Valor
    , qtdRec   :: Int
    , qtdDes   :: Int
    } deriving (Show, Eq)

-- (<>) combina dois resumos somando campo a campo
instance Semigroup Resumo where
    r1 <> r2 = Resumo
        { totalRec = totalRec r1 + totalRec r2
        , totalDes = totalDes r1 + totalDes r2
        , qtdRec   = qtdRec   r1 + qtdRec   r2
        , qtdDes   = qtdDes   r1 + qtdDes   r2
        }

-- mempty = elemento neutro: combinar com ele nao muda nada
instance Monoid Resumo where
    mempty = Resumo 0 0 0 0

transacaoParaResumo :: Transacao -> Resumo
transacaoParaResumo t =
    case tipo t of
        Receita -> Resumo (valor t) 0         1 0
        Despesa -> Resumo 0         (valor t) 0 1

-- foldMap = foldr (<>) mempty . map
-- Uma unica passada pela lista produz totais E quantidades juntos.
gerarResumo :: [Transacao] -> Resumo
gerarResumo = foldMap transacaoParaResumo

saldoDoResumo :: Resumo -> Valor
saldoDoResumo r = totalRec r - totalDes r

-- soma e contagem saem do mesmo Resumo:
-- nao precisa percorrer a lista duas vezes para tirar a media
mediaDoResumo :: Resumo -> Valor
mediaDoResumo r
    | qtdDes r == 0 = 0
    | otherwise     = totalDes r / fromIntegral (qtdDes r)


-- ============================================================
-- 3. FUNCTOR
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
receitas = filter ehReceita

-- list comprehension
despesas :: [Transacao] -> [Transacao]
despesas ts = [ t | t <- ts, ehDespesa t ]

-- recursao: soma da lista vazia e 0; senao, primeiro + soma do resto
somaValores :: [Transacao] -> Valor
somaValores []          = 0
somaValores (t : resto) = valor t + somaValores resto

-- recursao
contar :: [a] -> Int
contar []          = 0
contar (_ : resto) = 1 + contar resto

-- mesma soma de somaValores, agora com foldr e composicao (.)
totalReceitas :: [Transacao] -> Valor
totalReceitas = foldr (\t acc -> valor t + acc) 0 . receitas

totalDespesas :: [Transacao] -> Valor
totalDespesas = somaValores . despesas

calcularSaldo :: [Transacao] -> Valor
calcularSaldo ts = totalReceitas ts - totalDespesas ts

mediaDespesas :: [Transacao] -> Valor
mediaDespesas = mediaDoResumo . gerarResumo

-- Maybe: pode nao existir despesa nenhuma
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

-- compara categoria ignorando maiusculas/minusculas
mesmaCategoria :: Categoria -> Transacao -> Bool
mesmaCategoria cat t = minusculas (categoria t) == minusculas cat

buscarCategoria :: Categoria -> [Transacao] -> [Transacao]
buscarCategoria cat ts = [ t | t <- ts, mesmaCategoria cat t ]

-- nub remove as categorias repetidas
categoriasDeDespesa :: [Transacao] -> [Categoria]
categoriasDeDespesa ts = nub [ categoria t | t <- ts, ehDespesa t ]

totalDaCategoria :: Categoria -> [Transacao] -> Valor
totalDaCategoria cat ts =
    somaValores [ t | t <- ts, ehDespesa t, mesmaCategoria cat t ]

-- Functor: Relatorio Categoria -> Relatorio (Categoria, Valor)
gastosPorCategoria :: [Transacao] -> Relatorio (Categoria, Valor)
gastosPorCategoria ts =
    fmap (\cat -> (cat, totalDaCategoria cat ts))
         (Relatorio (categoriasDeDespesa ts))

-- do maior gasto para o menor
ordenarPorValor :: [(Categoria, Valor)] -> [(Categoria, Valor)]
ordenarPorValor = sortBy (\(_, a) (_, b) -> compare b a)


-- ============================================================
-- 5. VALIDACOES PURAS
-- Nothing = entrada invalida. Sao funcoes puras, entao dao para
-- testar direto no GHCi sem rodar o programa:
--   > valorPositivo "1250,50"   ==> Just 1250.5
--   > dataBR "30/02/2026"       ==> Nothing
-- ============================================================

-- recursao para separar dia, mes e ano
separarPorBarra :: String -> [String]
separarPorBarra texto =
    case break (== '/') texto of
        (parte, [])        -> [parte]
        (parte, _ : resto) -> parte : separarPorBarra resto

-- avaliacao preguicosa: o read so roda depois que all isDigit deu True
numeroEntre :: Int -> Int -> Int -> String -> Bool
numeroEntre tamanho minimo maximo texto =
    contar texto == tamanho
        && all isDigit texto
        && numero >= minimo
        && numero <= maximo
  where
    numero = read texto :: Int

-- calendario gregoriano
bissexto :: Int -> Bool
bissexto ano = (ano `mod` 4 == 0 && ano `mod` 100 /= 0) || ano `mod` 400 == 0

diasDoMes :: Int -> Int -> Int
diasDoMes mes ano
    | mes == 2 && bissexto ano = 29
    | mes == 2                 = 28
    | mes `elem` [4, 6, 9, 11] = 30
    | otherwise                = 31

-- pattern matching em [d, m, a] (dd/mm/aaaa)
dataValida :: DataBR -> Bool
dataValida texto =
    case separarPorBarra texto of
        [d, m, a] -> numeroEntre 2 1 31 d
                        && numeroEntre 2 1 12 m
                        && numeroEntre 4 1900 2100 a
                        && read d <= diasDoMes (read m) (read a)
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


-- ============================================================
-- 6. FORMATACAO (funcoes que so montam Strings)
-- ============================================================

formatarValor :: Valor -> String
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
-- 7. ENTRADA E SAIDA (IO)
-- ============================================================

lerLinha :: String -> IO String
lerLinha pergunta = do
    putStr pergunta
    hFlush stdout
    getLine

-- remove espacos das duas pontas
aparar :: String -> String
aparar = tirar . tirar
  where
    tirar = reverse . dropWhile isSpace

-- Uma unica funcao de leitura para todos os campos:
-- recebe o validador como parametro (funcao de alta ordem) e
-- repergunta por recursao ate o Maybe devolver Just.
lerCampo :: String -> (String -> Maybe a) -> String -> IO a
lerCampo pergunta valida erro = do
    entrada <- lerLinha pergunta
    case valida (aparar entrada) of
        Just x  -> return x
        Nothing -> do
            putStrLn ("  ! " ++ erro)
            lerCampo pergunta valida erro


-- ---------- Cadastro ----------

-- Imutabilidade: devolve uma lista NOVA, nada e alterado no lugar.
-- (ts ++ [nova]) e O(n), mas mantem a ordem cronologica e a lista
-- e pequena; usar (nova : ts) seria O(1) porem inverteria a ordem.
cadastrar :: TipoTransacao -> [Transacao] -> IO [Transacao]
cadastrar tp ts = do
    putStrLn ("\n--- Cadastro de " ++ show tp ++ " ---")
    d  <- lerCampo "Descricao         : " naoVazio      "Campo obrigatorio."
    v  <- lerCampo "Valor (R$)        : " valorPositivo "Valor invalido. Digite um numero maior que zero (ex.: 1250.50)."
    c  <- lerCampo "Categoria         : " naoVazio      "Campo obrigatorio."
    dt <- lerCampo "Data (dd/mm/aaaa) : " dataBR        "Data invalida. Use o formato dd/mm/aaaa (ex.: 05/06/2026)."
    putStrLn (">> " ++ show tp ++ " cadastrada com sucesso!")
    return (ts ++ [Transacao d v c dt tp])


-- ---------- Relatorios na tela ----------

listarTransacoes :: [Transacao] -> IO ()
listarTransacoes [] = putStrLn "Nenhuma transacao cadastrada ainda."
listarTransacoes ts = putStr (intercalate (linha ++ "\n") (map textoTransacao ts))

mostrarMaiorGasto :: [Transacao] -> IO ()
mostrarMaiorGasto ts =
    case maiorGasto ts of
        Nothing -> putStrLn "Nenhuma despesa cadastrada."
        Just t  -> do
            putStrLn ("Descricao: " ++ descricao t)
            putStrLn ("Categoria: " ++ categoria t)
            putStrLn ("Valor....: " ++ formatarValor (valor t))

mostrarGastosPorCategoria :: [Transacao] -> IO ()
mostrarGastosPorCategoria ts
    | null lista = putStrLn "Nenhuma despesa cadastrada."
    | otherwise  = putStr (unlines [ c ++ ": " ++ formatarValor v | (c, v) <- lista ])
  where
    lista = ordenarPorValor (itens (gastosPorCategoria ts))

mostrarTotal :: String -> Valor -> Int -> IO ()
mostrarTotal nome total qtd = do
    putStrLn ("Total de " ++ nome ++ " : " ++ formatarValor total)
    putStrLn ("Quantidade        : " ++ show qtd)

menuBuscarCategoria :: [Transacao] -> IO ()
menuBuscarCategoria ts = do
    cat <- lerCampo "Categoria que deseja buscar: " naoVazio "Campo obrigatorio."
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

-- todos os numeros saem de um unico Resumo (Monoid)
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
    putStrLn ("Media de gastos        : " ++ formatarValor (mediaDoResumo r))
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
-- 8. MENU E LOOP PRINCIPAL
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

-- opcoes que so mostram informacao (nao mudam a lista)
consultar :: String -> [Transacao] -> IO ()
consultar op ts =
    case op of
        "3"  -> do putStrLn "--- TODAS AS TRANSACOES ---"
                   listarTransacoes ts
        "4"  -> putStrLn ("Saldo atual: " ++ formatarValor (calcularSaldo ts))
        "5"  -> mostrarTotal "receitas" (totalReceitas ts) (contar (receitas ts))
        "6"  -> mostrarTotal "despesas" (totalDespesas ts) (contar (despesas ts))
        "7"  -> do putStrLn "--- MAIOR GASTO ---"
                   mostrarMaiorGasto ts
        "8"  -> do putStrLn ("Media de gastos   : " ++ formatarValor (mediaDespesas ts))
                   putStrLn ("Quantidade        : " ++ show (contar (despesas ts)))
        "9"  -> menuBuscarCategoria ts
        "10" -> relatorioCompleto ts
        _    -> putStrLn "Opcao invalida! Digite um numero de 0 a 10."

-- A lista e argumento da funcao: cada volta do loop recebe a lista nova.
-- E o "laco" do programa, feito com recursao.
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


-- ============================================================
-- 9. DADOS DE EXEMPLO E PONTO DE ENTRADA
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

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin  utf8
    putStrLn "Bem-vindo(a) ao Controle Financeiro Pessoal!"
    putStrLn "(o sistema ja inicia com algumas transacoes de exemplo)"
    loop transacoesExemplo
