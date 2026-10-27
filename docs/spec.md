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

Um literal sem `.` e sem expoente é um `int` (inteiro de 64 bits com sinal);
com `.` ou expoente é um `float` (`f64`). Então `1` é `int` e `1.0` e `1e3` são
`float`. Literal inteiro que não cabe em 64 bits é erro léxico.

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
+  -  *  /  //  %  ++  ==  !=  <  <=  >  >=  |>  <|  ??
\  ->  =  .  ?  ,  (  )  {  }  [  ]
```

O lexer é guloso: `//` e `??` são um token só. `?` sozinho é o acesso
opcional (4.1), então `t?k??0` vira `t ? k ?? 0` sem ambiguidade.

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
          | rpipe ;

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

(* pipe reverso: o lado direito é uma expressão completa *)
rpipe     = pipe [ "<|" expr ] ;
pipe      = or_expr  { "|>" or_expr } ;
or_expr   = and_expr { "or" and_expr } ;
and_expr  = cmp_expr { "and" cmp_expr } ;
cmp_expr  = coalesce [ cmp_op coalesce ] ;
cmp_op    = "==" | "!=" | "<" | "<=" | ">" | ">=" ;
coalesce  = concat [ "??" coalesce ] ;
concat    = add_expr [ "++" concat ] ;
add_expr  = mul_expr { ("+" | "-") mul_expr } ;
mul_expr  = unary { ("*" | "/" | "//" | "%") unary } ;
unary     = ("-" | "not") unary
          | app ;
app       = postfix { postfix } ;
postfix   = atom { access } ;
access    = ( "." | "?" ) ident
          | ( "." | "?" ) "(" expr ")" ;

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
| 2     | `<\|`                             | direita         |
| 3     | `\|>`                             | esquerda        |
| 4     | `or`                              | esquerda        |
| 5     | `and`                             | esquerda        |
| 6     | `==` `!=` `<` `<=` `>` `>=`       | nenhuma (`a < b < c` é erro) |
| 7     | `??`                              | direita         |
| 8     | `++`                              | direita         |
| 9     | `+` `-`                           | esquerda        |
| 10    | `*` `/` `//` `%`                  | esquerda        |
| 11    | unários `-` `not`                 | prefixo         |
| 12    | aplicação `f x y`                 | esquerda        |
| 13    | acesso `t.k`, `t.(e)`, `t?k`, `t?(e)` | esquerda (pós-fixo) |

Consequências:

- `x |> f a |> g` é `g (f a x)`. O `|>` aplica o lado direito ao esquerdo:
  `x |> f` = `f x`.
- `f <| g <| x` é `f (g x)`, como o `$` do Haskell. O lado direito do `<|`
  é uma expressão completa, então pode ser um `let`, `if`, `case` ou lambda
  sem parênteses: `List.all <| List.map (\c -> c.ok) cases`,
  `with_default <| \x -> x + 1`.
- Misturando os dois, o `<|` liga mais fraco: `f <| x |> g` é `f (g x)`.
- O `?` toma o lugar do ponto: `t?x ?? 0` é `(t?x) ?? 0`.
- `a ?? 0 + 1` é `a ?? (0 + 1)`; `a ?? 0 == 1` é `(a ?? 0) == 1`.
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
| `int`      | `1`, `-42`                     | inteiro de 64 bits |
| `float`    | `2.5`, `1e9`                   | `f64` |
| `string`   | `"oi"`, `"x = \(x)"`           | imutável, UTF-8 |
| `list`     | `[1, 2, 3]`                    | sequência indexada |
| `table`    | `{ a = 1, ("b" ++ "c") = 2 }`  | mapa chave → valor |
| `function` | `\x -> x`, builtins            | |

Todos os valores são imutáveis.

### 4.1 Tables

- Chaves podem ser `string`, `int`, `float` ou `boolean`. Um `float` com valor
  inteiro é normalizado para `int` (`t.(1.0)` é `t.(1)`); `NaN` como chave é
  erro. Usar `nil`, uma lista, uma table ou uma função como chave é erro.
- `{ k = v }` com `k` identificador é açúcar para `{ ("k") = v }`.
- `{ k }` é açúcar para `{ k = k }`.
- `{ (e) = v }` usa o valor de `e` como chave (espelho de `t.(e)`). A chave é
  avaliada para montar a table; o valor continua lazy.
- Chave repetida em um literal é erro.
- A ordem de inserção é preservada (para impressão e para `keys`).
- `t.k` é açúcar para `t.("k")`.
- Acessar uma chave ausente com `t.k` ou `t.(e)` é **erro**
  (`chave "k" não existe`, com a posição).
- `t?k` e `t?(e)` são o acesso opcional: retornam `nil` se a chave não
  existir ou se `t` for `nil`. Então dá para encadear: `cfg?server?port`.
  Se `t` não for `nil` nem table (nem lista, para `?(i)`), continua sendo erro.
- `a ?? b` é `a` se `a` não for `nil`, e `b` caso contrário. O `b` só é
  avaliado se necessário: `cfg?port ?? 8080`.
- Um campo com valor `nil` existe como chave: decidir se a chave existe nunca
  força o valor. `{ a = nil }.a` é `nil`, sem erro.

### 4.2 Listas

- `[a, b, c]` cria uma lista de tamanho fixo com elementos lazy.
- Listas são um tipo separado de table.
- `l.(i)` acessa o elemento `i`, com índice começando em **0**. O índice deve ser
  `int` (um `float` com valor inteiro também é aceito). Índice fora do
  intervalo é erro; `l?(i)` retorna `nil` nesse caso.

### 4.3 Funções

- Lambdas são curried: `\x y -> e` é `\x -> \y -> e`. Aplicação parcial é
  natural: `List.map run_case` é uma função esperando a lista.
- Parâmetros são padrões (seção 6): `\{ast, result} -> …`.
- Aplicar um valor que não é função é erro.

### 4.4 Verdade

`if`, `and`, `or` e `not` exigem `boolean`; qualquer outro valor é erro. Não
existe "truthy": `if t?k then …` é erro quando o resultado é `nil`; use
`if t?k != nil then …`.

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

Abaixo, "número" é `int` ou `float`.

| Operador            | Operandos                  | Resultado |
|---------------------|----------------------------|-----------|
| `+ - *`             | número                     | `int` se os dois forem `int`; senão `float` |
| `/`                 | número                     | sempre `float` (`7 / 2` = `3.5`) |
| `//`                | número                     | divisão arredondada para baixo: `int` se os dois forem `int` (`7 // 2` = `3`, `-7 // 2` = `-4`); senão `float` (`7.5 // 2` = `3.0`) |
| `%`                 | número                     | resto de `//`, com o sinal do divisor (`-7 % 2` = `1`); mesma regra de tipo |
| `-x`                | número                     | mesmo tipo |
| `++`                | string ++ string, list ++ list | concatenação |
| `< <= > >=`         | número/número ou string/string | boolean (números por valor; strings em ordem de bytes) |
| `== !=`             | quaisquer                  | boolean |
| `??`                | qualquer, qualquer         | lado esquerdo se não for `nil`, senão o direito |
| `and or`            | boolean                    | boolean, curto-circuito |
| `not`               | boolean                    | boolean |
| `x \|> f`           | qualquer, função           | `f x` |
| `f <\| x`           | função, qualquer           | `f x` |

- Em operações com `int`, overflow é erro (`9223372036854775807 + 1`).
- `//` e `%` com divisor `int` zero são erro. Com `float`, a divisão segue
  IEEE 754 (`1 / 0` = `inf`, `0.0 / 0` = `NaN`).
- Não há conversão automática entre números e strings.
- `==` é igualdade estrutural: números por valor, strings por conteúdo, listas
  elemento a elemento, tables pelo mesmo conjunto de chaves com valores iguais.
  `int` e `float` são comparados por valor (`1 == 1.0` é `true`). Fora isso,
  valores de tipos diferentes são diferentes (`1 == "1"` é `false`). Comparar
  funções é erro. A comparação força só o necessário e para na primeira
  diferença.
- Qualquer outra combinação de tipos é erro, com os tipos na mensagem.

### 5.5 Interpolação

`"a \(e) b"` é equivalente a `"a " ++ to_string e ++ " b"`, onde `to_string`
converte `string` (sem aspas), `int`, `float`, `boolean` e `nil`. Um `float`
sempre aparece com parte decimal (`1.0`, `2.5`, `1e+20`). Interpolar list,
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
| `type_of`        | any → string             | `"nil"`, `"boolean"`, `"int"`, `"float"`, `"string"`, `"list"`, `"table"`, `"function"` |
| `length`         | list/string → int        | tamanho (strings em bytes na v0) |
| `keys`           | table → list             | chaves em ordem de inserção |
| `has`            | table → key → boolean    | se a chave existe |
| `to_string`      | any → string             | conversão da seção 5.5 |
| `parse_number`   | string → int/float/nil   | lê um literal numérico; `nil` se não for número |
| `floor`          | número → int             | arredonda para baixo (erro se não couber em `int`) |
| `to_float`       | número → float           | converte para `float` |

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

Guardas (combinar padrão e condição no mesmo ramo) ficam para depois da v0;
por enquanto, use um `if` aninhado no ramo.

---

## 10. Fora do escopo da v0

- **IO.** Sol é pura; a v0 só avalia e imprime. Depois virá um runtime de IO
  embutível, em que o programa hospedeiro define o protocolo (por exemplo, o
  valor do programa descreve requisições que o host executa).
- Imports remotos (URLs de verdade): a v0 só lê do disco.
- Captura de erros (`try`), tipos estáticos, módulos com interface explícita,
  guardas no `case`.

### 10.1 Saída de `sol eval`

`sol eval <arquivo>` força **profundamente** o valor do arquivo e o imprime na
sintaxe de Sol (strings com aspas, tables com `k = v` na ordem de inserção,
funções como `<function>`). Código de saída 0 em sucesso e 1 em erro
(mensagem em stderr).

---

## 11. Perguntas abertas

1. **Builtins:** globais (provisório) ou dentro de uma table?
2. **Lambda como último argumento:** permitir `List.map \x -> x + 1` sem
   parênteses (como o `BlockArguments` do Haskell)? Provisório: não, já que
   `<|` cobre a maior parte dos casos.
3. **Comentário de bloco:** nenhum (provisório) ou `{- … -}`?
