# Blackjack-Functional-Programming

![Project IMG](Blackjack.png)

## Programming Principles Project
**This Project was made in collaboration with two other classmates:**<br>
Pedro Simoes <br>

## About

Blackjack in Haskell functional programming language.
Simulates a game of blackjack against a bot where the player decides how to play his hands and how much to bet.

## Learned
 - Haskell
 - The essence of functional programming languages, their structure and why they are useful
 - Tests for Haskell, including properties and QuickCheck tools


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
