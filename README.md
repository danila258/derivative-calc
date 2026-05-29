# Derivative Calculator

Калькулятор производных с REPL-интерфейсом.

## 1. Установка

### Зависимости
- GHC (компилятор Haskell)
- Cabal (система сборки)

### Установка на Linux

Ubuntu/Debian:
```sh
sudo apt update
sudo apt install ghc cabal-install
```

### Установка на macOS (Homebrew)
```sh
brew install ghc cabal-install
```

### Проверка
```sh
ghc --version
cabal --version
```

## 2. Сборка

Из корня проекта выполните:
```sh
cabal build
```

## 3. Запуск

Запустите REPL:
```sh
cabal run exe:derivative-calc
```

Пример сессии:
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

Поддерживаемые операции:
- Базовые операторы: +, -, *, /, ^
- Тригонометрические функции: sin, cos, tan, cot
- Обратные тригонометрические функции: arcsin, arccos, arctan, arccot
- Логарифмы: log, ln
- Экспоненты: exp, e^x, a^x

## 4. Запуск тестов

```sh
cabal test
```
