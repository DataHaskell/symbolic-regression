{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Symbolic.Regression
import System.Random (mkStdGen)
import System.Process (callCommand)

feynmanOpts :: D.ReadOptions
feynmanOpts = D.defaultReadOptions
    { D.headerSpec = D.NoHeader
    , D.columnSeparator = ' '
    }

main :: IO ()
main = do
    let src = "/Users/mchavinda/code/Feynman_with_units/I.12.1"
        tmp = "/tmp/sr_profile_tmp.dat"
    callCommand $ "head -n 500 '" ++ src ++ "' | sed 's/ *$//' > '" ++ tmp ++ "'"
    df <- D.readSeparated feynmanOpts tmp
    let cols = D.columnNames df
        target = last cols
        cfg = defaultRegressionConfig
            { generations = 50
            , populationSize = 100
            , maxExpressionSize = 7
            , simplifyExpressions = True
            , showTrace = False
            }
    models <- fit (mkStdGen 42) cfg (F.col target) df
    putStrLn $ "Models found: " ++ show (length models)
