# Haskell Finance Manager

Projeto da disciplina de **Programação Funcional**, desenvolvido em **Haskell**, executado pelo terminal.

---

## Objetivo

Permitir que o usuário registre suas **receitas** e **despesas** e consulte informações
financeiras através de relatórios: saldo atual, totais, maior gasto, média de gastos,
gastos por categoria e listagem completa.

O objetivo acadêmico é demonstrar, em um programa, os principais
conceitos de programação funcional: funções puras, imutabilidade, recursão,
list comprehension, funções de ordem superior, pattern matching, guards,
tipos algébricos (`data`) e instâncias de classes de tipos (`Monoid` e `Functor`).

---

## Estrutura

```
projeto/
├── Main.hs            -- todo o código do sistema
├── README.md          -- este arquivo
```

Todo o código está em um único arquivo (`Main.hs`).

---

## Como compilar

```bash
ghc Main.hs -o controle
```

## Como executar

```bash
./controle
```

Ou, sem compilar (interpretado):

```bash
runghc Main.hs
```

> **Requisito:** apenas o **GHC** instalado. O projeto **não usa nenhuma biblioteca
> externa** — só módulos do pacote `base` (`Data.List`, `Data.Char`, `System.IO`,
> `Text.Printf`), que já vêm com o compilador.
>
> Instalação no macOS: `brew install ghc`
> Instalação geral (Linux/macOS/Windows): [ghcup](https://www.haskell.org/ghcup/)

---

## Menu do sistema

```
======== CONTROLE FINANCEIRO PESSOAL ========
 1 - Cadastrar receita
 2 - Cadastrar despesa
 3 - Listar todas as transacoes
 4 - Mostrar saldo atual
 5 - Mostrar total de receitas
 6 - Mostrar total de despesas
 7 - Mostrar maior gasto
 8 - Mostrar media de gastos
 9 - Buscar transacoes por categoria
10 - Relatorio completo
 0 - Sair
=============================================
```

---

## Validações de entrada

| Campo                 | Regra                                                                                                                                                            |
| --------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Descrição e categoria | não podem ficar vazios                                                                                                                                           |
| Valor                 | precisa ser número maior que zero; aceita vírgula ou ponto (`1250,50`)                                                                                           |
| Data                  | precisa estar no formato `dd/mm/aaaa`, com mês 1–12, ano 1900–2100 e dia **existente naquele mês** (`30/02` e `31/04` são recusados; `29/02` só em ano bissexto) |
| Opção do menu         | fora de 0–10 exibe "opção inválida" e volta ao menu                                                                                                              |

Em todos os casos o sistema **pergunta de novo** até a entrada ser válida, usando
recursão em vez de laço.

---

## Relatórios disponíveis

| Relatório                       | Como é calculado                                                     |
| ------------------------------- | -------------------------------------------------------------------- |
| Saldo atual                     | `totalReceitas - totalDespesas`                                      |
| Total de receitas               | `map` + `foldr` sobre as receitas                                    |
| Total de despesas               | soma **recursiva** das despesas                                      |
| Maior gasto                     | busca **recursiva** com guards (mostra descrição, categoria e valor) |
| Média de gastos                 | soma das despesas ÷ quantidade (com guard contra divisão por zero)   |
| Quantidade de receitas/despesas | contagem recursiva + campos do `Resumo` (Monoid)                     |
| Gastos por categoria            | list comprehension + `nub` + `fmap` (Functor)                        |
| Listagem completa               | impressão recursiva da lista                                         |

---

## Conceitos de Haskell utilizados

| Conceito                        | Onde aparece em `Main.hs`                                                                                                                                |
| ------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Tipo personalizado (`data`)** | `TipoTransacao`, `Transacao`, `Resumo`                                                                                                                   |
| **Record syntax**               | campos de `Transacao` (viram funções de acesso)                                                                                                          |
| **`deriving (Show, Eq)`**       | nos três tipos acima                                                                                                                                     |
| **Instância de `Monoid`**       | `instance Semigroup Resumo` / `instance Monoid Resumo`                                                                                                   |
| **Instância de `Functor`**      | `instance Functor Relatorio`                                                                                                                             |
| **Recursão**                    | `somaValores`, `contar`, `maiorGasto`, `separarPorBarra`, `listarTransacoes`, `imprimirCategorias`, `lerTexto`, `lerValor`, `lerData` e o próprio `loop` |
| **List comprehension**          | `despesas`, `buscarCategoria`, `categoriasDeDespesa`, `totalDaCategoria`                                                                                 |
| **`map`**                       | `totalReceitas`, `gerarResumo`, `minusculas`                                                                                                             |
| **`filter`**                    | `receitas`                                                                                                                                               |
| **`foldr`**                     | `totalReceitas`, `gerarResumo`                                                                                                                           |
| **Funções de ordem superior**   | `filter ehReceita`, `foldr (<>)`, `fmap (\cat -> ...)`, `sortBy (\...)`, `all isDigit`                                                                   |
| **Pattern matching**            | `somaValores []` / `(t:resto)`, `case tipo t of Receita -> ...`, `case ... of Nothing / Just`, `[d, m, a]` em `dataValida`                               |
| **Guards**                      | `mediaDespesas`, `maiorGasto`, `diasDoMes`, validação em `lerValor`                                                                                      |
| **Avaliação preguiçosa**        | `numeroEntre` — o `read` só roda depois do `all isDigit`                                                                                                 |
| **`Maybe`**                     | retorno de `maiorGasto` (pode não existir despesa)                                                                                                       |
| **Funções puras**               | toda a seção 4 e 5 — nenhuma delas faz `IO`                                                                                                              |
| **`IO`**                        | seções 6 e 7 (`main`, `loop`, cadastros e impressões)                                                                                                    |
| **Imutabilidade**               | `cadastrar` devolve uma lista **nova**; nada é alterado no lugar                                                                                         |

---

## Acentuação e codificação do terminal

O código-fonte foi escrito **sem acentos** para funcionar em qualquer terminal.
Ainda assim, `main` configura o programa para UTF-8:

```haskell
hSetEncoding stdout utf8
hSetEncoding stdin  utf8
```

Com isso, **digitar acentos nos campos funciona normalmente** (testado com
`Café da manhã` e categoria `Alimentação`, inclusive na busca em maiúsculas).

### ⚠️ No Windows (PowerShell / Prompt de Comando)

O console do Windows usa por padrão uma _code page_ legada (CP850 ou CP1252 em
português), enquanto o programa fala UTF-8. Esse descompasso causa dois sintomas:

- na **saída**, acentos aparecem embaralhados (`Ã§`, `Ã£`);
- na **entrada**, digitar acento pode derrubar o programa com
  `hGetLine: invalid argument (invalid byte sequence)`.

**Solução:** troque o console para UTF-8 antes de executar.

```powershell
chcp 65001
.\controle.exe
```

No **Windows Terminal** ou no **PowerShell 7** isso geralmente já vem configurado
e o programa roda direto. No macOS e no Linux não é preciso fazer nada — o
terminal já é UTF-8.

> Se não quiser depender disso, é só não usar acentos ao cadastrar: o sistema
> funciona igual, já que todos os textos do programa são ASCII.

---

## Melhorias futuras

- Salvar e carregar as transações em arquivo (`readFile` / `writeFile`).
- Separar o código em módulos (`Tipos.hs`, `Operacoes.hs`, `Main.hs`).
- Filtrar relatórios por mês/período usando um tipo `Data` próprio.
- Editar e remover transações.
- Testes automatizados com HUnit ou QuickCheck.
- Impedir datas futuras ou muito antigas comparando com a data atual do sistema.
