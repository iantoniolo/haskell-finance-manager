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


-- 4. EXIBICAO NA TELA (IO)
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


-- 5. DADOS DE EXEMPLO
transacoesIniciais :: [Transacao]
transacoesIniciais =
    [ Transacao "Salario"        3500.00 "Trabalho"     "05/06/2026" Receita
    , Transacao "Freelance"       800.00 "Trabalho"     "12/06/2026" Receita
    , Transacao "Mercado"         250.00 "Alimentacao"  "06/06/2026" Despesa
    , Transacao "Restaurante"     100.00 "Alimentacao"  "09/06/2026" Despesa
    , Transacao "Combustivel"     240.00 "Transporte"   "07/06/2026" Despesa
    , Transacao "Aluguel"        1200.00 "Moradia"      "10/06/2026" Despesa
    ]


-- 6. PONTO DE ENTRADA
main :: IO ()
main = do
    putStrLn ""
    putStrLn "Controle Financeiro Pessoal"
    putStrLn ""
    exibirTransacoes transacoesIniciais
    putStrLn ""
    putStrLn ("Quantidade de receitas : " ++ show (contar (receitas transacoesIniciais)))
    putStrLn ("Quantidade de despesas : " ++ show (contar (despesas transacoesIniciais)))
    putStrLn ("Total de receitas      : " ++ formatCurrency (totalReceitas transacoesIniciais))
    putStrLn ("Total de despesas      : " ++ formatCurrency (totalDespesas transacoesIniciais))
    putStrLn ("Saldo atual            : " ++ formatCurrency (calcularSaldo transacoesIniciais))
    putStrLn ("Media de gastos        : " ++ formatCurrency (mediaDespesas transacoesIniciais))
    putStrLn ""
    putStrLn "--- Maior gasto ---"
    exibirMaiorGasto transacoesIniciais
    putStrLn ""
