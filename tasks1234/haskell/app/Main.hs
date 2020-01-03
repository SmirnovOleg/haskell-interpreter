module Main where

import REPL
import Runtime (runtime, tvs, typedRuntime)

main :: IO ()
main = runRepl runtime tvs typedRuntime
