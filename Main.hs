-- Projeto da disciplina de Programacao Funcional
-- Sistema de Controle Financeiro Pessoal
--
-- Compilar: ghc Main.hs -o controle && ./controle
-- Executar: runghc Main.hs

module Main where

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
-- 2. FORMATACAO (funcoes puras que so montam Strings)
formatCurrency :: Double -> String
formatCurrency v  = printf "R$ %.2f" v

linha :: String
linha = replicate 45 '-'

textoTransacao :: Transacao -> String
textoTransacao t  =
    unlines
        [ show (tipo t)
        , "Descricao: " ++ descricao t
        , "Valor....: " ++ formatCurrency (valor t)
        , "Categoria: " ++ categoria t
        , "Data.....: " ++ dataDaTransacaoBR t
        ]


-- 3. EXIBICAO NA TELA (IO)
-- impressao recursiva
exibirTransacoes :: [Transacao] -> IO ()
exibirTransacoes []          = putStrLn "Nenhuma transacao cadastrada ainda."
exibirTransacoes [t]         = putStr (textoTransacao t)
exibirTransacoes (t : resto) = do
    putStr (textoTransacao t)
    putStrLn linha
    exibirTransacoes resto


-- 4. DADOS DE EXEMPLO
transacoesIniciais :: [Transacao]
transacoesIniciais =
    [ Transacao "Salario" 3500.00 "Trabalho"    "05/06/2026" Receita
    , Transacao "Mercado"  250.00 "Alimentacao" "06/06/2026" Despesa
    ]


-- 5. PONTO DE ENTRADA
main :: IO ()
main = do
    putStrLn "Controle Financeiro Pessoal"
    putStrLn ""
    exibirTransacoes transacoesIniciais
