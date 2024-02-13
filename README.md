# miniagda
rust implementation for a subset of the [agda](https://github.com/agda/agda) programming language[^0]

## features

### implemented
- indexed families of data types

### in progress
- definitions by dependent pattern matching

### planned
- implicit arguments 
- holes
- granular caching

## structure

```
crates/             -- rust code
├─ miniagda/          -- the actual language implementation
├─ miniagda-bin/      -- standalone binary to elaborate an agda file once with debugging options
└─ miniagda-watch/    -- standalone tui to actively work on an agda file
examples/           -- example agda files that can be elaborated using miniagda
materials/          -- presentation slides
scripts/            -- development and installation scripts
```

## literature

- J. Cockx: Dependent pattern matching and proof-relevant unification
- J. Cockx, A. Abel: Elaborating dependent (co)pattern matching
- A. Kovacs: Elaboration with first-class implicit function types
- A. Abel, B. Pientka: Higher-Order Dynamic Pattern Unification for Dependent Types and Records
- U. Norell: Towards a practical programming language based on dependent type theory

[^0]: masters project at the chair of programming languages @ uni freiburg