# Especificação da linguagem Sol — v0

Sol é uma linguagem funcional, pura, lazy e dinamicamente tipada, baseada em
tables (inspirada em Lua). Um programa Sol é um conjunto de arquivos; cada
arquivo é **uma expressão**. A v0 é um interpretador em Rust executado como:

```
sol eval ./Main.sol
```

que avalia a expressão do arquivo e imprime o valor resultante.

Este documento é a referência para o parser (passo 2) e o interpretador
(passo 3). Pontos marcados com **[Aberto]** são decisões provisórias que ainda
precisam da confirmação do André; a seção [Perguntas abertas](#11-perguntas-abertas)
junta todas elas.

---

## 1. Exemplo

```sol
-- imports
let List = import "sollang.org/Std/List.sol"

let ops =
  let add = \x y -> x + y
  let sub = \x y -> x - y
  in { add, sub }

let eval = \ast ->
  if ast.type == "literal" then
    ast.value
  if ast.type == "binop" then
    ops.(ast.op) (eval ast.lhs) (eval ast.rhs)
  else
    error "Unknown ast type \(ast.type)"

let test_eval =
  let cases = [
    { ast = { type = "literal", value = 1 }, result = 1 },
    {
      ast = {
        type = "binop",
        op = "add",
        lhs = { type = "literal", value = 1 },
        rhs = { type = "literal", value = 2 },
      },
      result = 3,
    },
  ]
  let run_case = \{ast, result} -> eval ast == result
  in cases
    |> List.map run_case
    |> List.all

in {
  eval,
  test_eval,
}
```

`eval` é recursiva: todas as definições de um mesmo bloco `let` enxergam umas
às outras (seção 5.2).

---

## 2. Léxico

### 2.1 Espaços e comentários

- Espaços, tabs e quebras de linha só separam tokens. **A sintaxe não depende
  de indentação.**
- `--` inicia um comentário que vai até o fim da linha.
- **[Aberto]** Comentário de bloco: nenhum na v0. Candidato: `{- ... -}`.

### 2.2 Identificadores

```
ident = [A-Za-z_][A-Za-z0-9_]*
```

O identificador `_` sozinho é reservado para o padrão coringa. A convenção da
Std é `snake_case` para valores e funções e `PascalCase` para módulos
(`List`, `Std`).

### 2.3 Palavras reservadas

```
let in if then else end case import true false nil and or not
```

### 2.4 Números

```
number = digits [ "." digits ] [ ("e" | "E") ["+" | "-"] digits ]
digits = [0-9] ( [0-9] | "_" )*
```

Exemplos: `0`, `42`, `3.14`, `1e9`, `1_000_000`. Não há literal negativo: `-1`
é o operador unário `-` aplicado a `1`.

**[Aberto]** Representação: a v0 usa um único tipo numérico (`f64`, como Lua
5.1). Números inteiros são impressos sem `.0`. Alternativa: inteiro e float
separados (como Lua 5.3).

### 2.5 Strings

Delimitadas por aspas duplas. Escapes:

| Escape  | Significado            |
|---------|------------------------|
| `\n`    | quebra de linha        |
| `\t`    | tab                    |
| `\r`    | retorno de carro       |
| `\\`    | barra invertida        |
| `\"`    | aspas                  |
| `\(e)`  | interpolação de `e`    |

Qualquer outro `\x` é erro léxico. Uma string pode conter quebras de linha
literais. A interpolação aceita qualquer expressão, inclusive com strings
dentro: `"total: \(show "x" n)"`. O lexer precisa contar parênteses para achar
o `)` que fecha a interpolação.

### 2.6 Operadores e pontuação

```
+  -  *  /  %  ++  ==  !=  <  <=  >  >=  |>
\  ->  =  .  ,  (  )  {  }  [  ]
```

**[Aberto]** Diferente: `!=` (provisório) ou `~=` (Lua).

---

## 3. Gramática

EBNF; `{ x }` é zero ou mais, `[ x ]` é opcional. Vírgula final é permitida em
tables, listas e padrões.

```ebnf
file      = expr EOF ;

expr      = let_expr
          | if_expr
          | case_expr
          | lambda
          | pipe ;

(* bloco let: uma ou mais definições seguidas de um único `in` *)
let_expr  = binding { binding } "in" expr ;
binding   = "let" pattern "=" expr ;

(* if encadeado: só o último ramo tem else; ou fecha com end *)
if_expr   = "if" expr "then" expr
            { "if" expr "then" expr }
            chain_end ;

(* case: os mesmos ramos do if, mas com padrões no lugar de condições *)
case_expr = "case" expr
            "if" pattern "then" expr
            { "if" pattern "then" expr }
            chain_end ;

chain_end = "else" expr                 (* ramo padrão *)
          | "end" ;                     (* sem ramo padrão: exaustivo *)

lambda    = "\" pattern { pattern } "->" expr ;

pipe      = or_expr  { "|>" or_expr } ;
or_expr   = and_expr { "or" and_expr } ;
and_expr  = cmp_expr { "and" cmp_expr } ;
cmp_expr  = concat [ cmp_op concat ] ;
cmp_op    = "==" | "!=" | "<" | "<=" | ">" | ">=" ;
concat    = add_expr [ "++" concat ] ;
add_expr  = mul_expr { ("+" | "-") mul_expr } ;
mul_expr  = unary { ("*" | "/" | "%") unary } ;
unary     = ("-" | "not") unary
          | app ;
app       = postfix { postfix } ;
postfix   = atom { "." ident | "." "(" expr ")" } ;

atom      = number | string | "true" | "false" | "nil"
          | ident
          | "(" expr ")"
          | table
          | list
          | "import" string_lit ;

table     = "{" [ field { "," field } [ "," ] ] "}" ;
field     = ident                       (* atalho: { add } = { add = add } *)
          | key "=" expr ;
key       = ident | string_lit | "(" expr ")" ;

list      = "[" [ expr { "," expr } [ "," ] ] "]" ;

pattern   = "_"
          | ident
          | literal                     (* só em case, seção 9 *)
          | table_pat
          | list_pat
          | "(" pattern ")" ;
table_pat = "{" [ field_pat { "," field_pat } [ "," ] ] "}" ;
field_pat = ident                       (* { ast } = { ast = ast } *)
          | key "=" pattern ;
list_pat  = "[" [ pattern { "," pattern } ] [ "," "..." ident ] "]" ;
literal   = number | string_lit | "true" | "false" | "nil" ;
```

`string_lit` é uma string sem interpolação (usada em `import` e em chaves de
padrões).

### 3.1 Precedência e associatividade

Da menor para a maior:

| Nível | Construção                        | Associatividade |
|-------|-----------------------------------|-----------------|
| 1     | `let … in`, `if`, `case`, `\x ->` | estende ao máximo para a direita |
| 2     | `\|>`                             | esquerda        |
| 3     | `or`                              | esquerda        |
| 4     | `and`                             | esquerda        |
| 5     | `==` `!=` `<` `<=` `>` `>=`       | nenhuma (`a < b < c` é erro) |
| 6     | `++`                              | direita         |
| 7     | `+` `-`                           | esquerda        |
| 8     | `*` `/` `%`                       | esquerda        |
| 9     | unários `-` `not`                 | prefixo         |
| 10    | aplicação `f x y`                 | esquerda        |
| 11    | acesso `t.k`, `t.(e)`             | esquerda (pós-fixo) |

Consequências:

- `x |> f a |> g` é `g (f a x)`. O `|>` aplica o lado direito ao esquerdo:
  `x |> f` = `f x`.
- `ops.(ast.op) ast.lhs ast.rhs` é `(ops.(ast.op)) (ast.lhs) (ast.rhs)`:
  o acesso liga mais forte que a aplicação.
- `f -1` é a subtração `f - 1`; para passar um negativo use `f (-1)`.
- `not a == b` é `(not a) == b`.
- `let`, `if`, `case` e lambdas só aparecem em posição de expressão completa (início
  de arquivo, corpo de `let`/lambda, ramos de `if`, dentro de parênteses,
  valores de table e itens de lista). Como argumento ou operando precisam de
  parênteses: `List.map (\x -> x + 1) xs`, `1 + (if c then 1 else 2)`.

### 3.2 Como as construções terminam

Sem indentação significativa, cada construção termina pelo próximo token que
não pode continuá-la:

- Uma aplicação para em qualquer palavra reservada, operador binário,
  `,`, `)`, `}`, `]` ou fim de arquivo.
- O valor de um `let x = …` termina no próximo `let` ou `in` que não pertença
  a uma construção interna. Por isso, no exemplo, o bloco interno
  `let add … let sub … in { add, sub }` termina em `{ add, sub }` e o
  `let eval` seguinte volta a ser do bloco externo.
- Um ramo `then e` termina no próximo `if` (novo ramo), `else` (último
  ramo) ou `end` (fim da cadeia).
- `if` e `case` aninhados num ramo `then` consomem os ramos seguintes até o
  seu próprio `else` ou `end`. Quando o aninhado fecha com `end`, não há
  ambiguidade e os ramos seguintes voltam para o externo:

  ```sol
  case ast
    if { type = "num", value } then
      case value
        if 0 then "zero"
      end                     -- fecha o case interno
    if { type = "str" } then "string"
  end
  ```

  Se o aninhado fecha com `else e`, o `e` termina no primeiro token que não o
  continua, então um `if` seguinte também volta para o externo:
  `if a then (if b then x else y) if c then z end` pode ser escrito sem os
  parênteses. Os parênteses só são necessários quando o próprio `e` é outro
  `if`/`case` aberto.

---

## 4. Valores

Sol tem tipagem dinâmica. Os tipos de valor são:

| Tipo       | Exemplos                       | Observação |
|------------|--------------------------------|------------|
| `nil`      | `nil`                          | ausência de valor |
| `boolean`  | `true`, `false`                | |
| `number`   | `1`, `2.5`                     | ver 2.4 |
| `string`   | `"oi"`, `"x = \(x)"`           | imutável, UTF-8 |
| `list`     | `[1, 2, 3]`                    | sequência indexada |
| `table`    | `{ a = 1, ("b" ++ "c") = 2 }`  | mapa chave → valor |
| `function` | `\x -> x`, builtins            | |

Todos os valores são imutáveis.

### 4.1 Tables

- Chaves podem ser `string`, `number` ou `boolean`. Usar `nil`, uma lista,
  uma table ou uma função como chave é erro.
- `{ k = v }` com `k` identificador é açúcar para `{ ("k") = v }`.
- `{ k }` é açúcar para `{ k = k }`.
- `{ (e) = v }` usa o valor de `e` como chave (espelho de `t.(e)`). A chave é
  avaliada para montar a table; o valor continua lazy.
- Chave repetida em um literal é erro.
- A ordem de inserção é preservada (para impressão e para `keys`).
- `t.k` é açúcar para `t.("k")`.
- **[Aberto]** Acessar uma chave ausente retorna `nil` (como Lua). Diferente de
  Lua, um campo com valor `nil` continua existindo como chave: decidir se a
  chave existe nunca força o valor. Alternativa: chave ausente é erro.

### 4.2 Listas

- `[a, b, c]` cria uma lista de tamanho fixo com elementos lazy.
- `l.(i)` acessa o elemento `i`. Índice fora do intervalo é erro.
- **[Aberto]** Base do índice: 0 (provisório) ou 1 (como Lua).
- **[Aberto]** Listas são um tipo separado de table (provisório) ou tables com
  chaves inteiras, como em Lua. Tipo separado deixa a implementação mais
  simples e as mensagens de erro mais claras.

### 4.3 Funções

- Lambdas são curried: `\x y -> e` é `\x -> \y -> e`. Aplicação parcial é
  natural: `List.map run_case` é uma função esperando a lista.
- Parâmetros são padrões (seção 6): `\{ast, result} -> …`.
- Aplicar um valor que não é função é erro.

### 4.4 Verdade

**[Aberto]** `if`, `and`, `or` e `not` exigem `boolean`; qualquer outro valor é
erro (provisório). Alternativa Lua: `nil` e `false` são falsos e o resto é
verdadeiro.

---

## 5. Semântica

### 5.1 Pureza e preguiça

- Não há efeitos colaterais. A mesma expressão no mesmo ambiente sempre dá o
  mesmo valor.
- A avaliação é **call-by-need**: toda expressão vira um thunk, avaliado no
  máximo uma vez, só quando o valor é necessário. São lazy: argumentos de
  função, valores de `let`, valores de campos de table, elementos de lista e o
  resultado de `import`.
- Forçam avaliação: aplicar uma função (força a função, não o argumento), os
  operadores aritméticos e de comparação, o teste de `if`, `and`/`or` (só o
  necessário: curto-circuito), acesso a campo (força a table e a chave),
  casamento de padrão (só as partes inspecionadas), interpolação e a impressão
  final.
- Um thunk que precisa de si mesmo para terminar (`let x = x + 1 in x`) gera o
  erro `ciclo de avaliação detectado` em vez de travar.

### 5.2 `let` e recursão

- Um bloco `let a = … let b = … in corpo` define todos os nomes ao mesmo tempo,
  com escopo recursivo: qualquer valor do bloco pode usar qualquer nome do
  bloco, inclusive o próprio. Isso dá recursão e recursão mútua.
- A ordem das definições não importa para o significado.
- Definir o mesmo nome duas vezes no mesmo bloco é erro. Um bloco interno pode
  sombrear um nome externo.
- `let {add, sub} = ops in …` desestrutura (seção 6). Os nomes ligados por
  padrão também são recursivos e lazy: `ops` só é forçado quando `add` ou `sub`
  forem usados.

### 5.3 `if`

`if c1 then e1 if c2 then e2 … else en` avalia `c1`; se verdadeiro, o
resultado é `e1`; senão, passa para `c2`, e assim por diante; se nenhum
for verdadeiro, o resultado é `en`.

Se a cadeia fecha com `end` em vez de `else`, ela é **exaustiva**: se nenhuma
condição for verdadeira, é erro em tempo de execução
(`nenhum ramo do if foi escolhido`, com a posição do `if`). Como tudo é lazy,
o erro só acontece se o valor for forçado.

```sol
let sign = \n ->
  if n > 0 then 1
  if n < 0 then -1
  if n == 0 then 0
  end
```

### 5.4 Operadores

| Operador            | Operandos                  | Resultado |
|---------------------|----------------------------|-----------|
| `+ - * /`           | number                     | number |
| `%`                 | number                     | resto com o sinal do divisor (como Lua) |
| `-x`                | number                     | number |
| `++`                | string ++ string, list ++ list | concatenação |
| `< <= > >=`         | number/number ou string/string | boolean (strings em ordem de bytes) |
| `== !=`             | quaisquer                  | boolean |
| `and or`            | boolean                    | boolean, curto-circuito |
| `not`               | boolean                    | boolean |
| `x \|> f`           | qualquer, função           | `f x` |

- Divisão por zero segue IEEE 754 (`1/0` = `inf`).
- `==` é igualdade estrutural: números por valor, strings por conteúdo, listas
  elemento a elemento, tables pelo mesmo conjunto de chaves com valores iguais.
  Valores de tipos diferentes são diferentes (`1 == "1"` é `false`). Comparar
  funções é erro. A comparação força só o necessário e para na primeira
  diferença.
- Qualquer outra combinação de tipos é erro, com os tipos na mensagem.

### 5.5 Interpolação

`"a \(e) b"` é equivalente a `"a " ++ to_string e ++ " b"`, onde `to_string`
converte `string` (sem aspas), `number`, `boolean` e `nil`. Interpolar list,
table ou função é erro na v0.

### 5.6 Erros

- `error msg` é uma função builtin. Como tudo é lazy, ela só falha quando seu
  resultado é forçado: `let x = error "boom" in 1` resulta em `1`.
- `msg` deve ser string.
- Na v0 erros não podem ser capturados dentro da linguagem; eles abortam a
  avaliação e `sol eval` imprime:

  ```
  erro: Unknown ast type foo
    em ./Main.sol:13:5
    forçado por ./Main.sol:28:40
  ```

  Isso inclui a posição de origem da expressão que falhou e a cadeia de
  thunks que levaram até ela.

---

## 6. Padrões (desestruturação)

Padrões aparecem em parâmetros de lambda e em `let`, e na v0 são
**irrefutáveis**: se não casarem, é erro em tempo de execução.

| Padrão               | Casa com                                   | Liga |
|----------------------|--------------------------------------------|------|
| `x`                  | qualquer valor (sem forçar)                | `x` |
| `_`                  | qualquer valor (sem forçar)                | nada |
| `{a, b}`             | table com as chaves `a` e `b`              | `a`, `b` |
| `{a = p, "c d" = q}` | table com as chaves; casa `p` e `q` com os valores | o que `p`, `q` ligarem |
| `[p, q]`             | lista de tamanho exatamente 2             | o que `p`, `q` ligarem |
| `[p, ...rest]`       | lista de tamanho ≥ 1                       | `p` e `rest` (o resto) |

- Padrões de table são abertos: chaves extras são ignoradas.
- O casamento é lazy: os nomes só forçam o valor desestruturado quando são
  usados. `\{ast, result} -> …` aplicada a algo que não é table só falha
  quando `ast` ou `result` forem usados.

Padrões literais (`1`, `"binop"`) e padrões refutáveis existem só no `case`
(seção 9).

---

## 7. Imports

```sol
let List = import "sollang.org/Std/List.sol"
let Util = import "./util.sol"
```

- `import` é uma palavra reservada seguida de uma string literal (sem
  interpolação), então o grafo de imports pode ser resolvido sem avaliar nada.
- O valor de `import "p"` é o valor da expressão do arquivo `p`.
- **Resolução na v0** (só caminhos locais):
  - caminhos que começam com `./` ou `../` são relativos ao diretório do
    arquivo que faz o import;
  - qualquer outro caminho (como `sollang.org/Std/List.sol`) é procurado,
    na ordem, em cada diretório da variável de ambiente `SOL_PATH`
    (separados por `:`), e por último no diretório `lib/` que acompanha o
    interpretador. A Std fica em `lib/sollang.org/Std/`.
  - caminhos absolutos (`/…`) são erro.
- Cada arquivo é carregado e analisado uma vez por execução, identificado pelo
  caminho canônico; imports repetidos devolvem o mesmo valor.
- Como o resultado de um import é lazy, imports cíclicos são permitidos desde
  que nenhum valor dependa de si mesmo; se depender, vale o erro de ciclo da
  seção 5.1.
- Erro de arquivo inexistente ou de sintaxe em um import é reportado com a
  posição do `import`.

---

## 8. Builtins

Disponíveis em todo arquivo como nomes globais (podem ser sombreados). O
mínimo para escrever a Std em Sol:

| Nome             | Tipo                     | Descrição |
|------------------|--------------------------|-----------|
| `error`          | string → ⊥               | aborta a avaliação (5.6) |
| `type_of`        | any → string             | `"nil"`, `"boolean"`, `"number"`, `"string"`, `"list"`, `"table"`, `"function"` |
| `length`         | list/string → number     | tamanho (strings em bytes na v0) |
| `keys`           | table → list             | chaves em ordem de inserção |
| `has`            | table → key → boolean    | se a chave existe |
| `to_string`      | any → string             | conversão da seção 5.5 |
| `to_number`      | string → number/nil      | `nil` se não for número |
| `floor`          | number → number          | arredonda para baixo |

`map`, `filter`, `fold`, `all` etc. ficam na Std (`List.sol`), escritos em Sol
(passo 4).

**[Aberto]** Globais soltos (provisório) ou agrupados numa table, como
`Builtin.length`.

---

## 9. `case`

`case` é o `if` encadeado com padrões no lugar de condições:

```sol
let eval = \ast ->
  case ast
    if { type = "literal", value } then value
    if { type = "binop", op, lhs, rhs } then ops.(op) (eval lhs) (eval rhs)
    if { type } then error "Unknown ast type \(type)"
  end
```

- `case x` avalia `x` sob demanda e tenta os ramos em ordem; o primeiro padrão
  que casar escolhe o ramo, e os nomes ligados pelo padrão valem só nele.
- Os padrões são os da seção 6, acrescidos de literais (`1`, `"binop"`,
  `true`, `nil`), que casam por `==`. Dentro de um padrão de table, um literal
  restringe o valor do campo: `{ type = "literal" }` casa só se `type` existir
  e for igual a `"literal"`.
- Aqui os padrões são **refutáveis**: se um não casar, passa para o próximo
  ramo. Um padrão de table contra algo que não é table, ou de lista contra
  algo que não é lista, simplesmente não casa.
- O casamento força só o necessário para decidir: no exemplo, `ast` e
  `ast.type` são forçados, mas `value`, `lhs` e `rhs` não.
- Um identificador num padrão sempre **liga** um nome novo, nunca compara com
  uma variável existente. Para comparar com um valor calculado, use `if`.
- Fechamento igual ao `if`:
  - `else e` é o ramo padrão, sem padrão nem ligações;
  - `end` torna o `case` exaustivo: se nenhum ramo casar, é erro em tempo de
    execução (`nenhum ramo do case casou`, com a posição do `case` e o tipo
    do valor).

```sol
let describe = \n ->
  case n
    if 0 then "zero"
    if 1 then "um"
    else "muitos"

let sum = \xs ->
  case xs
    if [] then 0
    if [x, ...rest] then x + sum rest
  end
```

**[Aberto]** Guardas, para combinar padrão e condição no mesmo ramo. Candidato:

```sol
case n
  if { type = "num", value } when value < 0 then "negativo"
  if { type = "num" } then "não negativo"
end
```

(acrescentaria a palavra `when`). Sem guardas, o mesmo se escreve com um `if`
aninhado no ramo.

---

## 10. Fora do escopo da v0

- **IO.** Sol é pura; a v0 só avalia e imprime. Depois virá um runtime de IO
  embutível, em que o programa hospedeiro define o protocolo (por exemplo, o
  valor do programa descreve requisições que o host executa).
- Imports remotos (URLs de verdade): a v0 só lê do disco.
- Captura de erros (`try`), tipos estáticos, módulos com interface explícita.

### 10.1 Saída de `sol eval`

`sol eval <arquivo>` força **profundamente** o valor do arquivo e o imprime na
sintaxe de Sol (strings com aspas, tables com `k = v` na ordem de inserção,
funções como `<function>`). Código de saída 0 em sucesso e 1 em erro
(mensagem em stderr).

---

## 11. Perguntas abertas

1. **Guardas no `case`:** `if p when c then …` (seção 9) ou nenhuma na v0?
2. **Chave ausente:** `t.k` retorna `nil` (provisório) ou dá erro?
3. **Listas:** tipo separado (provisório) ou tables com chaves inteiras? Índice
   base 0 (provisório) ou 1?
4. **Números:** só `f64` (provisório) ou inteiro + float?
5. **Verdade:** `if`/`and`/`or` exigem boolean (provisório) ou seguem Lua
   (`nil` e `false` são falsos)?
6. **Diferente:** `!=` (provisório) ou `~=`?
7. **Builtins:** globais (provisório) ou dentro de uma table?
8. **Lambda como último argumento:** permitir `List.map \x -> x + 1` sem
   parênteses (como o `BlockArguments` do Haskell)? Provisório: não.
9. **Comentário de bloco:** nenhum (provisório) ou `{- … -}`?
