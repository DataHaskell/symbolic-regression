{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException, evaluate, try)
import Data.List (isPrefixOf, sort)
import Data.Maybe (catMaybes)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Symbolic.Regression
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.IO (IOMode (..), hFlush, hPutStrLn, stdout, withFile)
import System.Process (callCommand)
import System.Random (mkStdGen)
import Text.Printf (hPrintf, printf)

feynmanOpts :: D.ReadOptions
feynmanOpts =
    D.defaultReadOptions
        { D.headerSpec = D.NoHeader
        , D.columnSeparator = ' '
        }

benchConfig :: Int -> Int -> Int -> Bool -> Bool -> RegressionConfig
benchConfig gens pop maxSz useEGraph useMultiset =
    defaultRegressionConfig
        { generations = gens
        , populationSize = pop
        , maxExpressionSize = maxSz
        , simplifyExpressions = useEGraph
        , useMultisetContainers = useMultiset
        , showTrace = False
        }

readSubsampled :: Int -> FilePath -> FilePath -> IO D.DataFrame
readSubsampled nRows src dst = do
    callCommand $
        "head -n "
            ++ show nRows
            ++ " '"
            ++ src
            ++ "' | sed 's/ *$//' > '"
            ++ dst
            ++ "'"
    D.readSeparated feynmanOpts dst

computeR2 :: [Double] -> [Double] -> Double
computeR2 actual predicted =
    let n = fromIntegral (length actual) :: Double
        mean_y = sum actual / n
        ss_tot = sum [(y - mean_y) ^ (2 :: Int) | y <- actual]
        ss_res = sum [(y - yp) ^ (2 :: Int) | (y, yp) <- zip actual predicted]
     in if ss_tot == 0 then 1.0 else 1.0 - ss_res / ss_tot

data Result = Result
    { rName :: String
    , rNVars :: Int
    , rNModels :: Int
    , rTime :: Double
    , rR2 :: Double
    , rFormula :: String
    }
    deriving (Show)

runOne ::
    Int ->
    Int ->
    Int ->
    Int ->
    Bool ->
    Bool ->
    FilePath ->
    FilePath ->
    IO Result
runOne gens pop maxSz seed useEGraph useMultiset src tmp = do
    df <- readSubsampled 500 src tmp
    let cols = D.columnNames df
        nVars = length cols - 1
        target = last cols
        cfg = benchConfig gens pop maxSz useEGraph useMultiset
    t0 <- getCurrentTime
    models <- fit (mkStdGen seed) cfg (F.col target) df
    t1 <- getCurrentTime
    let elapsed = realToFrac (diffUTCTime t1 t0) :: Double
        best = last models -- most complex = best fit
        predicted = D.columnAsList best (D.derive "pred" best df)
        actual = D.columnAsList (F.col target) df
        r2 = computeR2 actual predicted
        formula = D.prettyPrint best
    _ <- evaluate r2 -- force
    return $ Result "" nVars (length models) elapsed r2 formula

writeMarkdown ::
    FilePath -> Int -> Int -> Int -> Int -> Int -> [Result] -> Double -> IO ()
writeMarkdown path rows gens pop maxSz seed results totalTime =
    withFile path WriteMode $ \h -> do
        hPutStrLn h "# Feynman Benchmark Results"
        hPutStrLn h ""
        hPrintf
            h
            "**Config:** %d generations, %d population, max size %d, seed %d, %d rows/dataset\n"
            gens
            pop
            maxSz
            seed
            rows
        hPrintf
            h
            "**Total time:** %.1fs | **Avg:** %.2fs/problem\n"
            totalTime
            (totalTime / fromIntegral (length results))
        hPutStrLn h ""
        hPutStrLn h "| # | Dataset | Vars | Models | R² | Time (s) | Best Formula |"
        hPutStrLn h "|---|---------|------|--------|----|----------|--------------|"
        mapM_
            ( \(i, r) ->
                hPrintf
                    h
                    "| %d | %s | %d | %d | %.4f | %.2f | `%s` |\n"
                    (i :: Int)
                    (rName r)
                    (rNVars r)
                    (rNModels r)
                    (rR2 r)
                    (rTime r)
                    (rFormula r)
            )
            (zip [1 ..] results)
        hPutStrLn h ""
        let goodR2 = length [r | r <- results, rR2 r >= 0.99]
            okR2 = length [r | r <- results, rR2 r >= 0.90, rR2 r < 0.99]
            poorR2 = length [r | r <- results, rR2 r < 0.90]
        hPutStrLn h "## Summary"
        hPutStrLn h ""
        hPrintf h "- R² >= 0.99: **%d** / %d\n" goodR2 (length results)
        hPrintf h "- R² >= 0.90: **%d** / %d\n" (goodR2 + okR2) (length results)
        hPrintf h "- R² < 0.90: **%d** / %d\n" poorR2 (length results)

writeTsv :: FilePath -> [Result] -> IO ()
writeTsv path results =
    withFile path WriteMode $ \h -> do
        hPutStrLn h "dataset\tvars\tmodels\tr2\ttime_seconds\tformula"
        mapM_
            ( \r ->
                hPrintf
                    h
                    "%s\t%d\t%d\t%.10f\t%.10f\t%s\n"
                    (rName r)
                    (rNVars r)
                    (rNModels r)
                    (rR2 r)
                    (rTime r)
                    (sanitize (rFormula r))
            )
            results
  where
    sanitize = map (\c -> if c == '\t' || c == '\n' || c == '\r' then ' ' else c)

data Options = Options
    { optPath :: FilePath
    , optRows :: Int
    , optGens :: Int
    , optPop :: Int
    , optMaxSz :: Int
    , optSeed :: Int
    , optUseEGraph :: Bool
    , optUseMultiset :: Bool
    , optMarkdownOut :: FilePath
    , optTsvOut :: Maybe FilePath
    }

defaultOptions :: FilePath -> Options
defaultOptions path =
    Options
        { optPath = path
        , optRows = 500
        , optGens = 50
        , optPop = 100
        , optMaxSz = 7
        , optSeed = 42
        , optUseEGraph = True
        , optUseMultiset = True
        , optMarkdownOut = "bench/feynman_results.md"
        , optTsvOut = Nothing
        }

usage :: String
usage =
    "Usage: feynman-bench <feynman_dir_or_file> [gens pop maxsz seed] "
        ++ "[--rows N] [--markdown-out FILE] [--tsv-out FILE] [--no-egraph] [--no-multiset]"

parseArgs :: [String] -> Options
parseArgs args = case args of
    [] -> error usage
    (path : rest) -> finish (go (defaultOptions path, []) rest)
  where
    go acc [] = acc
    go (opts, pos) ("--no-egraph" : xs) = go (opts{optUseEGraph = False}, pos) xs
    go (opts, pos) ("--no-multiset" : xs) = go (opts{optUseMultiset = False}, pos) xs
    go (opts, pos) ("--rows" : n : xs) = go (opts{optRows = read n}, pos) xs
    go (opts, pos) ("--markdown-out" : file : xs) = go (opts{optMarkdownOut = file}, pos) xs
    go (opts, pos) ("--tsv-out" : file : xs) = go (opts{optTsvOut = Just file}, pos) xs
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

resolveInputs :: FilePath -> IO [(String, FilePath)]
resolveInputs path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            files <- sort . filter (not . isPrefixOf ".") <$> listDirectory path
            pure [(file, path ++ "/" ++ file) | file <- files]
        else pure [(takeFileName path, path)]
  where
    takeFileName = reverse . takeWhile (/= '/') . reverse

main :: IO ()
main = do
    opts <- parseArgs <$> getArgs
    inputs <- resolveInputs (optPath opts)
    let nFiles = length inputs
        tmp = "/tmp/sr_feynman_tmp.dat"

    printf
        "%-20s %5s %6s %8s %8s\n"
        ("Dataset" :: String)
        ("Vars" :: String)
        ("Mod" :: String)
        ("R²" :: String)
        ("Time" :: String)
    printf "%s\n" (replicate 52 '-')

    totalT0 <- getCurrentTime
    results <-
        mapM
            ( \(i, (name, path)) -> do
                result <-
                    try
                        ( runOne
                            (optGens opts)
                            (optPop opts)
                            (optMaxSz opts)
                            (optSeed opts)
                            (optUseEGraph opts)
                            (optUseMultiset opts)
                            path
                            tmp
                        ) ::
                        IO (Either SomeException Result)
                case result of
                    Right r -> do
                        let r' = r{rName = name}
                        printf
                            "%-20s %5d %6d %8.4f %7.2fs  [%d/%d]\n"
                            name
                            (rNVars r')
                            (rNModels r')
                            (rR2 r')
                            (rTime r')
                            i
                            nFiles
                        hFlush stdout
                        return (Just r')
                    Left e -> do
                        printf
                            "%-20s %5s %6s %8s %8s  [%d/%d] %s\n"
                            name
                            ("" :: String)
                            ("ERR" :: String)
                            ("" :: String)
                            ("" :: String)
                            i
                            nFiles
                            (take 60 (show e))
                        hFlush stdout
                        return Nothing
            )
            (zip ([1 :: Int ..]) inputs)

    totalT1 <- getCurrentTime
    let totalElapsed = realToFrac (diffUTCTime totalT1 totalT0) :: Double
        good = catMaybes results

    writeMarkdown
        (optMarkdownOut opts)
        (optRows opts)
        (optGens opts)
        (optPop opts)
        (optMaxSz opts)
        (optSeed opts)
        good
        totalElapsed
    maybe (pure ()) (`writeTsv` good) (optTsvOut opts)
    printf
        "\n%d/%d succeeded | total %.1fs | avg %.2fs/problem\n"
        (length good)
        nFiles
        totalElapsed
        (totalElapsed / fromIntegral (max 1 (length good)))
    printf "Results written to %s\n" (optMarkdownOut opts)
