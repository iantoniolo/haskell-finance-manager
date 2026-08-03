-- Projeto da disciplina de Programacao Funcional
-- Sistema de Controle Financeiro Pessoal
--
-- Compilar: ghc Main.hs -o controle && ./controle
-- Executar: runghc Main.hs

module Main where

import Data.Char   (isSpace)
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
-- 2. FUNCOES
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


-- ============================================================
-- 3. FORMATACAO (funcoes que montam Strings)
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
-- 4. ENTRADA E SAIDA (IO)
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
    d  <- lerTexto "Descricao........: "
    v  <- lerValor "Valor (R$).......: "
    c  <- lerTexto "Categoria........: "
    dt <- lerTexto "Data (dd/mm/aaaa): "
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


-- ============================================================
-- 5. DADOS DE EXEMPLO
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
-- 6. PONTO DE ENTRADA
-- ============================================================

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin  utf8
    putStrLn ""
    putStrLn "Controle Financeiro Pessoal"
    putStrLn ""
    -- teste do cadastro: pede uma receita e uma despesa novas
    ts1 <- adicionarReceita transacoesExemplo
    ts2 <- adicionarDespesa ts1
    putStrLn ""
    listarTransacoes transacoesExemplo
    putStrLn ""
    putStrLn ("Quantidade de receitas : " ++ show (contar (receitas transacoesExemplo)))
    putStrLn ("Quantidade de despesas : " ++ show (contar (despesas transacoesExemplo)))
    putStrLn ("Total de receitas      : " ++ formatarValor (totalReceitas transacoesExemplo))
    putStrLn ("Total de despesas      : " ++ formatarValor (totalDespesas transacoesExemplo))
    putStrLn ("Saldo atual            : " ++ formatarValor (calcularSaldo transacoesExemplo))
    putStrLn ("Media de gastos        : " ++ formatarValor (mediaDespesas transacoesExemplo))
    putStrLn ""
    putStrLn "--- Maior gasto ---"
    mostrarMaiorGasto transacoesExemplo
    putStrLn ""
