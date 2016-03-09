# Haxl test generated graphs


## Installation and execution instructions

1. Install [rand-code-graph](https://github.com/goens/rand-code-graph)
- Install `shelly` and `text` with `cabal install shelly text`
- Change `scripts/RunTests.hs`
    1. Change `graphGenerationBinaryLocation` to the location of the compiled executable from rand-code-graph
    - set `graphsToGenerate` to the amount of graphs you want to generate **per-level**
    - set `maxLevel` to the maximum level you want to generate graphs for.
- Run the tests with `runhaskell scripts/RunTests.hs`

Results will be written to `results/results.json`
