# Blackjack-Functional-Programming

## Programming Principles Project

## About

Blackjack in Haskell functional programming language.
Simulates a game of blackjack against a bot where the player decides how to play his hands and how much to bet.

## Learned
 - Haskell
 - The essence of functional programming languages, their structure and why they are useful
 - Tests for Haskell, including properties and QuickCheck tools

---
## Operations

```bash
apostar [amountOfCreditsToBet]
```

```bash
sair
```

```bash
stand
```

```bash
hit
```


---
## Compilation

```bash
stack ghc Main.hs
```


---
## Execution

```bash
./Main [cardDeckFile]
```

```bash
./Main -n [amountOfCardDecks]
```

Above option for a random card deck with [amountOfCardDecks] decks shuffled

```bash
./Main -t
```

Above for testing
