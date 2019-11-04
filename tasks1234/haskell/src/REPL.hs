module REPL where

import AST
import Interpreter
import qualified Data.Map as Map


runtime :: Env
runtime = Map.fromList []