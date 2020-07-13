# Tetris in Haskell

![](.github/gameplay.gif)

## To play
Due to the `gloss library`'s outdated dependencies, `cabal run` will not work.
Suggested course of action is to install the dependencies in `hasktris.cabal`,
and to run `ghc --make` in the `src` directory.

## Feature Extensions (TODO)
* Levels
* Show next blocks before falling
* Randomize starting position of each block