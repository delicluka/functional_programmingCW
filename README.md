# Connect Four

This is a program that implements the popular Connect Four game.
You play the game against a computer player.

## Running the game

To run the game, run the following on the command line:
```
$ ghci ConnectFour.hs
```
When the program is loaded, run `main` and the game will start.
You can adjust the AI difficulty and choose who plays first.

No language extensions are necessary to make the program work.

## Rules

In Connect Four, two players take turns to insert `O` and `X` tokens into a 6x7
board, with the goal of creating a sequence of 4 consecutive tokens
(horizontally, vertically or diagonally).
When a token is inserted into a column, it falls down as much as possible.
For example, inserting `O` into the third column of the board
```
.......
.......
.......
.......
.XO....
.XOXO..
```
gives the outcome:
```
.......
.......
.......
..O....
.XO....
.XOXO..
```

The game ends when the whole board is full or a player manages to get a sequence
of 4 of the same symbol.

## AI

The computer player is implemented using the Minimax/Maximin algorithm.
It looks ahead a certain number of moves and sees whether there is a path that
guarantees victory.
If it cannot determine that some sequence of moves results in victory, it
estimates how good a board is using a heuristic function.

The Minimax algorithm is implemented in the `playAI` function.
Scoring the board is done with the `computeScore` function.

## Main challenges

I encountered two main challenges when I was working on this program:

* *Programming with monads.*
I had to become more comfortable with using the `IO` monad and `do` notation in
Haskell, especially when interpreting player input.
* *Heuristic for the AI.*
I knew from the start that I should use the Minimax algorithm for the AI.
Devising a heuristic was not too difficult, the main issues were implementation
and testing whether the heuristic made sense.

Luka Delić, 10 January 2021
