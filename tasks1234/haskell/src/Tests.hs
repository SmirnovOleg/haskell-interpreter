{-# LANGUAGE QuasiQuotes #-}

module Tests where

import NeatInterpolation
import Data.Text (Text)

firstTest :: Text
firstTest = [text|
                myFunc a b = a + b
                newF c = myFunc 3 5 + c
            |]