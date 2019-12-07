{-# LANGUAGE QuasiQuotes #-}

module Examples where

import NeatInterpolation (text)
import Data.Text
import Parser
import Text.Megaparsec     
            
fib =
    [text|
      fibonacci n = if (n == 1) then 1 else fibonacci (n-1) + fibonacci (n-2)
    |]
  
fact =
    [text|
        fact n = helper 1 n where
            helper acc 1 = acc
            helper acc n = helper (acc * n) (n - 1)
    |]
  
main :: IO ()
main = do
      treeParser (unpack fib)
      treeParser (unpack fact)      