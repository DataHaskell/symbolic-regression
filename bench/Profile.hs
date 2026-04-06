{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Symbolic.Regression
import System.Environment (getArgs)
import System.Process (callCommand)
import System.Random (mkStdGen)

feynmanOpts :: D.ReadOptions
feynmanOpts =
    D.defaultReadOptions
        { D.headerSpec = D.NoHeader
        , D.columnSeparator = ' '
        }

data Options = Options
    { optDataset :: FilePath
    , optRows :: Int
    , optGens :: Int
    , optPop :: Int
    , optMaxSz :: Int
    , optSeed :: Int
    , optUseEGraph :: Bool
    }

defaultOptions :: Options
defaultOptions =
    Options
        { optDataset = "/Users/mchavinda/code/Feynman_with_units/I.12.1"
        , optRows = 500
        , optGens = 50
        , optPop = 100
        , optMaxSz = 7
        , optSeed = 42
        , optUseEGraph = True
        }

usage :: String
usage =
    "Usage: sr-profile <dataset> [gens pop maxsz seed] [--rows N] [--no-egraph]"

parseArgs :: [String] -> Options
parseArgs [] = defaultOptions
parseArgs (dataset : rest) = finish (go (defaultOptions{optDataset = dataset}, []) rest)
  where
    go acc [] = acc
    go (opts, pos) ("--no-egraph" : xs) = go (opts{optUseEGraph = False}, pos) xs
    go (opts, pos) ("--rows" : n : xs) = go (opts{optRows = read n}, pos) xs
    go (opts, pos) (x : xs)
        | take 2 x == "--" = error usage
        | otherwise = go (opts, pos ++ [x]) xs

    finish (opts, []) = opts
    finish (opts, [g, p, m, s]) =
        opts
            { optGens = read g
            , optPop = read p
            , optMaxSz = read m
            , optSeed = read s
            }
    finish _ = error usage

main :: IO ()
main = do
    opts <- parseArgs <$> getArgs
    let src = optDataset opts
        tmp = "/tmp/sr_profile_tmp.dat"
    callCommand $
        "head -n "
            ++ show (optRows opts)
            ++ " '"
            ++ src
            ++ "' | sed 's/ *$//' > '"
            ++ tmp
            ++ "'"
    df <- D.readSeparated feynmanOpts tmp
    let cols = D.columnNames df
        target = last cols
        cfg =
            defaultRegressionConfig
                { generations = optGens opts
                , populationSize = optPop opts
                , maxExpressionSize = optMaxSz opts
                , simplifyExpressions = optUseEGraph opts
                , showTrace = False
                }
    models <- fit (mkStdGen (optSeed opts)) cfg (F.col target) df
    putStrLn $ "Models found: " ++ show (length models)
