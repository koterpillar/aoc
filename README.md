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

```shell
./create_day

# specific year, next day
./create_day 2020

# specific year and day
./create_day 2020 6
```

## Formatting

```shell
hs-format-changed
```
