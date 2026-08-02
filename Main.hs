-- Projeto da disciplina de Programacao Funcional
-- Sistema de Controle Financeiro Pessoal
--
-- Compilar: ghc Main.hs -o controle && ./controle
-- Executar: runghc Main.hs

module Main where

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
-- 4. EXIBICAO NA TELA (IO)
-- ============================================================

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
    putStrLn ""
    putStrLn "Controle Financeiro Pessoal"
    putStrLn ""
    listarTransacoes transacoesExemplo
    putStrLn ""
    putStrLn ("Quantidade de receitas : " ++ show (contar (receitas transacoesExemplo)))
    putStrLn ("Quantidade de despesas : " ++ show (contar (despesas transacoesExemplo)))
    putStrLn ("Total de receitas      : " ++ formatarValor (totalReceitas transacoesExemplo))
    putStrLn ("Total de despesas      : " ++ formatarValor (totalDespesas transacoesExemplo))
    putStrLn ("Saldo atual            : " ++ formatarValor (calcularSaldo transacoesExemplo))
