-- Sistema de Controle Financeiro Pessoal
-- Projeto da disciplina de Programacao Funcional -- Haskell puro (so o pacote base)
--
-- Compilar: ghc Main.hs -o controle && ./controle
-- Executar: runghc Main.hs

module Main where


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
-- 2. DADOS DE EXEMPLO
transacoesIniciais :: [Transacao]
transacoesIniciais =
    [ Transacao "Salario" 3500.00 "Trabalho"    "05/06/2026" Receita
    , Transacao "Mercado"  250.00 "Alimentacao" "06/06/2026" Despesa
    ]


-- 3. PONTO DE ENTRADA
main :: IO ()
main = do
    putStrLn "Controle Financeiro Pessoal"
    mapM_ print transacoesIniciais
