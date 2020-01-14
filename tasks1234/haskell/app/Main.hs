module Main where

import REPL
import Runtime (runtime)

main :: IO ()
main = runRepl runtime
