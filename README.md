# Advent of Code

## Credentials

Log in to [AOC](https://adventofcode.com/), open browser inspector, Storage,
Cookies and copy the _value_ of `session` cookie to `.session-cookie`.

## Running

```shell
stack build --file-watch --exec main

# specific year, last day
stack build --file-watch --exec 'main 2017'

# specific year and day
stack build --file-watch --exec 'main 2017 05'

# See main/Main.hs for more
```

## New day

Create `src/Y20YY/DayDD.hs` with the following content:

```haskell
module Y20YY.DayDD where

import AOC
import Utils

solver = error "write solver"

tasks = Tasks
    20YY
    DD
    ShowExamples
    (error "parser, see Miniparse")
    [Task solver (error "example result")]
```

## Formatting

```shell
hs-format-changed
```
