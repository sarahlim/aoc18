# Advent of Code 2018

Solutions to [Advent of Code 2018](https://adventofcode.com/2018).

For convenience, solutions are written entirely in the `test` directory. Input files downloaded from the AoC website are stored in the `input` directory, titled with the day number.

## Parse input files

To run the Day 1 solution with input file `input/1`:

```
stack runhaskell -- test/Day1Spec.hs < input/1
```

## Build tests

```
stack test --haddock-deps --fast --file-watch
```
