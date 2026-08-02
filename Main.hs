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
-- 2. FORMATACAO (funcoes puras que so montam Strings)
-- ============================================================

formatarValor :: Double -> String
formatarValor v = printf "R$ %.2f" v

linha :: String
linha = replicate 45 '-'

textoTransacao :: Transacao -> String
textoTransacao t =
    unlines
        [ show (tipo t)
        , "Descricao: " ++ descricao t
        , "Valor....: " ++ formatarValor (valor t)
        , "Categoria: " ++ categoria t
        , "Data.....: " ++ dataTransacao t
        ]


-- ============================================================
-- 3. EXIBICAO NA TELA (IO)
-- ============================================================

-- impressao recursiva
listarTransacoes :: [Transacao] -> IO ()
listarTransacoes []          = putStrLn "Nenhuma transacao cadastrada ainda."
listarTransacoes [t]         = putStr (textoTransacao t)
listarTransacoes (t : resto) = do
    putStr (textoTransacao t)
    putStrLn linha
    listarTransacoes resto


-- ============================================================
-- 4. DADOS DE EXEMPLO
-- ============================================================

transacoesExemplo :: [Transacao]
transacoesExemplo =
    [ Transacao "Salario" 3500.00 "Trabalho"    "05/06/2026" Receita
    , Transacao "Mercado"  250.00 "Alimentacao" "06/06/2026" Despesa
    ]


-- ============================================================
-- 5. PONTO DE ENTRADA
-- ============================================================

main :: IO ()
main = do
    putStrLn "Controle Financeiro Pessoal"
    putStrLn ""
    listarTransacoes transacoesExemplo
