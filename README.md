# Derivative Calculator

Symbolic derivative calculator with a REPL.

## 1. Setup

### Dependencies
- GHC (Haskell compiler)
- Cabal (build tool)

### macOS install (Homebrew)
```sh
brew install ghc cabal-install
```

### Linux install

Ubuntu/Debian:
```sh
sudo apt update
sudo apt install ghc cabal-install
```

### Verify
```sh
ghc --version
cabal --version
```

## 2. Build

From the project root:
```sh
cabal build
```

## 3. Run

Run the REPL:
```sh
cabal run exe:derivative-calc
```

Example session:
```
>F(x) = x^2 + (1/x)
>F(10)
100.1
>F'(10)
19.99
>F
x^2 + 1/x
>F'
2*x - 1/x^2
>exit
```

Supported operations include:
- Basic operators: +, -, *, /, ^
- Trigonometric: sin, cos, tan, cot
- Inverse trig: arcsin, arccos, arctan, arccot
- Logarithms: log, ln
- Exponentials: exp, e^x, a^x

## 4. Run tests

```sh
cabal test
```
