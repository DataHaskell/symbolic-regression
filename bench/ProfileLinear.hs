{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Symbolic.Regression
import System.Random (mkStdGen, randomRs)

default (Int, Double)

main :: IO ()
main = do
    let xs = take 100 $ randomRs (1 :: Double, 100) (mkStdGen 53)
        ys = take 100 $ randomRs (1 :: Double, 100) (mkStdGen 54)
        df0 =
            D.fromNamedColumns
                [ ("x", D.fromList xs)
                , ("y", D.fromList ys)
                ]
        x = F.col "x" :: D.Expr Double
        y = F.col "y" :: D.Expr Double
        df = D.derive "z" (F.pow x 2 + y) df0
        cfg =
            defaultRegressionConfig
                { generations = 50
                , populationSize = 50
                , maxExpressionSize = 7
                , numIslands = 3
                , showTrace = False
                }
    models <- fit (mkStdGen 1) cfg (F.col "z") df
    mapM_ (putStrLn . ("  Model: " ++) . D.prettyPrint) models
    putStrLn $ "Models found: " ++ show (length models)
