# Sol

Sol é uma linguagem de programação funcional, pura, lazy e dinamicamente
tipada, baseada em tables (inspirada em Lua). Cada arquivo é uma expressão.

```sol
let List = import "sollang.org/Std/List.sol"
let double = \x -> x * 2
in [1, 2, 3] |> List.map double
```

- Especificação v0: [docs/spec.md](docs/spec.md)
- O parser antigo (2021) está na branch `legacy-parser`.
