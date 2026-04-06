{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Symbolic.Regression
import System.Random (mkStdGen)
import System.Environment (getArgs)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

main :: IO ()
main = do
    args <- getArgs
    let (path, gens, pop, maxSz, seed) = case args of
          [p,g,ps,ms,s] -> (p, read g, read ps, read ms, read s)
          [p]           -> (p, 50, 100, 7, 42)
          _             -> error "Usage: bench <csv> [gens pop maxsz seed]"
    df <- D.readCsv path
    let target = last (D.columnNames df)
        cfg = defaultRegressionConfig
            { generations = gens, populationSize = pop
            , maxExpressionSize = maxSz, showTrace = True }
    putStrLn $ "Dataset: " ++ path ++ " (" ++ show (D.nRows df) ++ " rows)"
    t0 <- getCurrentTime
    models <- fit (mkStdGen seed) cfg (F.col target) df
    t1 <- getCurrentTime
    putStrLn $ "\nPareto: " ++ show (length models) ++ " models in " ++ show (diffUTCTime t1 t0)
    mapM_ (\m -> putStrLn $ "  " ++ D.prettyPrint m) models
