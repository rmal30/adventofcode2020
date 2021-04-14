# Advent of Code 2020

Solutions to the exercises at <https://adventofcode.com/2020/> in Haskell.

Inputs for each day are read from a text file (ie. 8.txt for Day 8) in the inputs directory.

## Building code for a particular day

```
ghc -static -O2 src/Utils.hs src/<Day number>.hs -Wall -Werror
```