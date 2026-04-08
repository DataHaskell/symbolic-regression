{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException, evaluate, try)
import Data.List (isPrefixOf, sort)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import DataFrame.Operations.Core (columnAsUnboxedVector)
import Symbolic.Regression.Boosting
import Symbolic.Regression.Expr (Features, PVector)
import Symbolic.Regression.Expr.Print (showExprWithVars)
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

-- | Extract feature matrix and target vector from a DataFrame.
dfToDataSet :: D.DataFrame -> T.Text -> (Features, PVector)
dfToDataSet df targetCol =
    let cols = D.columnNames df
        featureCols = filter (/= targetCol) cols
        nRows = D.nRows df
        mkCol c = case columnAsUnboxedVector (F.col c) df of
            Right v -> v
            Left _ -> VU.replicate nRows 0.0
        colVecs = map mkCol featureCols
        nFeats = length featureCols
        xss =
            V.fromListN nRows $
                [ VU.fromListN nFeats [colVecs !! f `VU.unsafeIndex` r | f <- [0 .. nFeats - 1]]
                | r <- [0 .. nRows - 1]
                ]
        yTarget = mkCol targetCol
     in (xss, yTarget)

data Result = Result
    { rName :: String
    , rNVars :: Int
    , rNTerms :: Int
    , rTime :: Double
    , rR2 :: Double
    , rFormula :: String
    }
    deriving (Show)

runOne :: Options -> FilePath -> FilePath -> IO Result
runOne opts src tmp = do
    df <- readSubsampled 500 src tmp
    let cols = D.columnNames df
        nVars = length cols - 1
        target = last cols
        (xTrain, yTrain) = dfToDataSet df target
        trainData = (xTrain, yTrain)
        valData = (xTrain, yTrain)
        cfg =
            defaultBoostConfig
                { bcRounds = optRounds opts
                , bcMaxWeakLearnerSize = optMaxSize opts
                , bcCandidatesPerRound = optCandidates opts
                , bcNloptIters = optNloptIters opts
                , bcLearningRate = optLearningRate opts
                , bcShowTrace = False
                }
    t0 <- getCurrentTime
    result <- fitBoosted (mkStdGen (optSeed opts)) cfg trainData valData
    t1 <- getCurrentTime
    let elapsed = realToFrac (diffUTCTime t1 t0) :: Double
        nTerms = length (brTerms result)
        featureNames = ["x" ++ show i | i <- [0 :: Int .. nVars - 1]]
        formula = case brDistilled result of
            (d : _) -> showExprWithVars featureNames d
            [] -> showExprWithVars featureNames (brEnsembleExpr result)
        trainMSE = brTrainMSE result
        yMean = VU.sum yTrain / fromIntegral (VU.length yTrain)
        ssTot = VU.sum (VU.map (\y -> (y - yMean) ** 2) yTrain)
        r2 =
            if ssTot == 0
                then 1.0
                else 1.0 - trainMSE * fromIntegral (VU.length yTrain) / ssTot
    _ <- evaluate r2
    return $ Result "" nVars nTerms elapsed r2 formula

writeMarkdown :: FilePath -> Int -> Int -> Int -> [Result] -> Double -> IO ()
writeMarkdown path rows rounds seed results totalTime =
    withFile path WriteMode $ \h -> do
        hPutStrLn h "# Boost Benchmark Results"
        hPutStrLn h ""
        hPrintf h "**Config:** %d rounds, seed %d, %d rows/dataset\n" rounds seed rows
        hPrintf
            h
            "**Total time:** %.1fs | **Avg:** %.2fs/problem\n"
            totalTime
            (totalTime / fromIntegral (length results))
        hPutStrLn h ""
        hPutStrLn h "| # | Dataset | Vars | Terms | R² | Time (s) | Formula |"
        hPutStrLn h "|---|---------|------|-------|----|----------|---------|"
        mapM_
            ( \(i, r) ->
                hPrintf
                    h
                    "| %d | %s | %d | %d | %.4f | %.2f | `%s` |\n"
                    (i :: Int)
                    (rName r)
                    (rNVars r)
                    (rNTerms r)
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

data Options = Options
    { optPath :: FilePath
    , optRows :: Int
    , optRounds :: Int
    , optSeed :: Int
    , optMaxSize :: Int
    , optCandidates :: Int
    , optNloptIters :: Int
    , optLearningRate :: Double
    , optMarkdownOut :: FilePath
    }

defaultOptions :: FilePath -> Options
defaultOptions path =
    Options
        { optPath = path
        , optRows = 500
        , optRounds = 200
        , optSeed = 42
        , optMaxSize = 7
        , optCandidates = 50
        , optNloptIters = 30
        , optLearningRate = 0.1
        , optMarkdownOut = "bench/boost_results.md"
        }

usage :: String
usage =
    "Usage: boost-bench <feynman_dir_or_file> [--rounds N] [--rows N] [--seed N] "
        ++ "[--maxsize N] [--candidates N] [--nlopt-iters N] [--lr F] [--markdown-out FILE]"

parseArgs :: [String] -> Options
parseArgs args = case args of
    [] -> error usage
    (path : rest) -> go (defaultOptions path) rest
  where
    go opts [] = opts
    go opts ("--rounds" : n : xs) = go opts{optRounds = read n} xs
    go opts ("--rows" : n : xs) = go opts{optRows = read n} xs
    go opts ("--seed" : n : xs) = go opts{optSeed = read n} xs
    go opts ("--maxsize" : n : xs) = go opts{optMaxSize = read n} xs
    go opts ("--candidates" : n : xs) = go opts{optCandidates = read n} xs
    go opts ("--nlopt-iters" : n : xs) = go opts{optNloptIters = read n} xs
    go opts ("--lr" : n : xs) = go opts{optLearningRate = read n} xs
    go opts ("--markdown-out" : file : xs) = go opts{optMarkdownOut = file} xs
    go _ _ = error usage

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
        tmp = "/tmp/sr_boost_bench_tmp.dat"

    printf
        "%-20s %5s %6s %8s %8s\n"
        ("Dataset" :: String)
        ("Vars" :: String)
        ("Terms" :: String)
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
                            opts
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
                            (rNTerms r')
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
        (optRounds opts)
        (optSeed opts)
        good
        totalElapsed
    printf
        "\n%d/%d succeeded | total %.1fs | avg %.2fs/problem\n"
        (length good)
        nFiles
        totalElapsed
        (totalElapsed / fromIntegral (max 1 (length good)))
    printf "Results written to %s\n" (optMarkdownOut opts)
