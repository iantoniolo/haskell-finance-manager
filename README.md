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

| Campo                 | Regra                                                                                                                                                            | Função pura     |
| --------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------- |
| Descrição e categoria | não podem ficar vazios                                                                                                                                           | `naoVazio`      |
| Valor                 | precisa ser número maior que zero; aceita vírgula ou ponto (`1250,50`)                                                                                           | `valorPositivo` |
| Data                  | precisa estar no formato `dd/mm/aaaa`, com mês 1–12, ano 1900–2100 e dia **existente naquele mês** (`30/02` e `31/04` são recusados; `29/02` só em ano bissexto) | `dataBR`        |
| Opção do menu         | fora de 0–10 exibe "opção inválida" e volta ao menu                                                                                                              | —               |

As regras são **funções puras** que devolvem `Maybe`: `Nothing` significa entrada
inválida. Como não fazem `IO`, dá para testá-las direto no GHCi:

```haskell
ghci> valorPositivo "1250,50"    -- Just 1250.5
ghci> valorPositivo "abc"        -- Nothing
ghci> dataBR "29/02/2024"        -- Just "29/02/2024"   (ano bissexto)
ghci> dataBR "29/02/2025"        -- Nothing
```

Quem fala com o usuário é uma **única** função, `lerCampo`, que recebe o validador
como parâmetro (função de alta ordem) e **pergunta de novo por recursão** até o
`Maybe` vir `Just`.

---

## Relatórios disponíveis

| Relatório                       | Como é calculado                                                     |
| ------------------------------- | -------------------------------------------------------------------- |
| Saldo atual                     | `totalReceitas - totalDespesas`                                      |
| Total de receitas               | `foldr` + composição (`.`) sobre as receitas                         |
| Total de despesas               | soma **recursiva** das despesas                                      |
| Maior gasto                     | busca **recursiva** com guards (mostra descrição, categoria e valor) |
| Média de gastos                 | vem do `Resumo`: `totalDes / qtdDes` — **uma única passada**          |
| Quantidade de receitas/despesas | campos do `Resumo` (Monoid) + contagem recursiva                     |
| Gastos por categoria            | list comprehension + `nub` + `fmap` (Functor)                        |
| Listagem completa               | `map` + `intercalate` para separar os registros                      |

### O papel do `Monoid` no cálculo da média

O `Resumo` guarda **soma e quantidade juntas**. Como ele é um `Monoid`, o relatório
inteiro sai de um único `foldMap`:

```haskell
gerarResumo = foldMap transacaoParaResumo   -- == foldr (<>) mempty . map
```

Isso importa na média. O caminho ingênuo percorre a lista **duas vezes** (uma para
somar, outra para contar):

```haskell
mediaDespesas ts = somaValores ds / fromIntegral (contar ds)   -- 2 travessias
```

Com o `Monoid`, uma passada só já traz os dois números:

```haskell
mediaDoResumo r = totalDes r / fromIntegral (qtdDes r)         -- 1 travessia
```

As leis do `Monoid` podem ser conferidas no GHCi:

```haskell
ghci> let (a, b) = splitAt 3 transacoesExemplo
ghci> gerarResumo (a ++ b) == gerarResumo a <> gerarResumo b   -- True (associatividade)
ghci> gerarResumo [] == mempty                                 -- True (elemento neutro)
```

A primeira igualdade é o que permitiria dividir a lista em pedaços, resumir cada
pedaço separadamente e combinar os resultados no fim.

---

## Conceitos de Haskell utilizados

| Conceito                        | Onde aparece em `Main.hs`                                                                                                    |
| ------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| **Apelido de tipo (`type`)**    | `Categoria`, `DataBR`, `Valor`                                                                                               |
| **Tipo soma (`data`)**          | `TipoTransacao` (`Receita` \| `Despesa`)                                                                                     |
| **Tipo produto (`data`)**       | `Transacao`, `Resumo`                                                                                                        |
| **`newtype`**                   | `Relatorio a`                                                                                                                |
| **Record syntax**               | campos de `Transacao` e `Resumo` (viram funções de acesso)                                                                   |
| **`deriving (Show, Eq)`**       | `TipoTransacao`, `Transacao`, `Resumo`                                                                                       |
| **Instância de `Monoid`**       | `instance Semigroup Resumo` / `instance Monoid Resumo`                                                                       |
| **Instância de `Functor`**      | `instance Functor Relatorio`                                                                                                 |
| **`foldMap`**                   | `gerarResumo` — evita percorrer a lista duas vezes                                                                           |
| **Recursão**                    | `somaValores`, `contar`, `maiorGasto`, `separarPorBarra`, `lerCampo` e o próprio `loop`                                      |
| **List comprehension**          | `despesas`, `buscarCategoria`, `categoriasDeDespesa`, `totalDaCategoria`, `mostrarGastosPorCategoria`                        |
| **`map`**                       | `minusculas`, `listarTransacoes`, `fmap` de `Relatorio`                                                                      |
| **`filter`**                    | `receitas`                                                                                                                   |
| **`foldr`**                     | `totalReceitas` (e dentro do `foldMap`)                                                                                      |
| **Composição (`.`)**            | `totalDespesas`, `mediaDespesas`, `totalReceitas`, `aparar`                                                                  |
| **Funções de ordem superior**   | `lerCampo` (recebe o validador), `filter ehReceita`, `fmap (\cat -> ...)`, `sortBy (\...)`, `all isDigit`                    |
| **Pattern matching**            | `somaValores []` / `(t:resto)`, `case tipo t of Receita -> ...`, `Nothing` / `Just`, `[d, m, a]` em `dataValida`             |
| **Guards**                      | `mediaDoResumo`, `maiorGasto`, `diasDoMes`, `dataBR`, `mostrarGastosPorCategoria`                                            |
| **Avaliação preguiçosa**        | `numeroEntre` — o `read` só roda depois do `all isDigit`                                                                     |
| **`Maybe`**                     | `maiorGasto` (pode não existir despesa) e os três validadores                                                                |
| **Funções puras**               | seções 4, 5 e 6 — nenhuma delas faz `IO`                                                                                     |
| **`IO`**                        | seções 7 e 8 (`main`, `loop`, `lerCampo`, cadastro e impressões)                                                             |
| **Imutabilidade**               | `cadastrar` devolve uma lista **nova**; nada é alterado no lugar                                                             |

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

## Decisões de projeto

- **`ts ++ [nova]` no cadastro** ([`cadastrar`](Main.hs)) é O(n), porque em estruturas
  persistentes a concatenação copia o caminho até o ponto alterado. Usar `nova : ts`
  seria O(1), mas inverteria a ordem cronológica da listagem. Como a lista é pequena,
  consideramos melhor manter a legibilidade.
- **Uma só função de leitura.** `lerCampo` recebe o validador como parâmetro, então
  não existem `lerTexto`/`lerValor`/`lerData` separadas repetindo a mesma estrutura.
- **`Either` não foi usado.** Ele daria mensagens de erro tipadas (em vez de `Maybe` +
  texto solto), mas aumentaria o código sem mudar o comportamento. O projeto priorizou
  ficar enxuto.

---

## Melhorias futuras

- Salvar e carregar as transações em arquivo (`readFile` / `writeFile`).
- Separar o código em módulos (`Tipos.hs`, `Operacoes.hs`, `Main.hs`).
- Filtrar relatórios por mês/período usando um tipo `Data` próprio.
- Editar e remover transações.
- Testes automatizados com HUnit ou QuickCheck.
- Impedir datas futuras ou muito antigas comparando com a data atual do sistema.
