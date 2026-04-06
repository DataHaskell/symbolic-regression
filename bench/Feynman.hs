{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Symbolic.Regression
import System.Random (mkStdGen)
import System.Environment (getArgs)
import System.Directory (listDirectory)
import System.Process (callCommand)
import System.IO (hFlush, stdout, withFile, IOMode(..), hPutStrLn)
import Data.List (sort, isPrefixOf, intercalate)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Control.Exception (try, SomeException, evaluate)
import Text.Printf (printf, hPrintf)

feynmanOpts :: D.ReadOptions
feynmanOpts = D.defaultReadOptions
    { D.headerSpec = D.NoHeader
    , D.columnSeparator = ' '
    }

benchConfig :: Int -> Int -> Int -> Bool -> RegressionConfig
benchConfig gens pop maxSz useEGraph = defaultRegressionConfig
    { generations = gens
    , populationSize = pop
    , maxExpressionSize = maxSz
    , simplifyExpressions = useEGraph
    , showTrace = False
    }

readSubsampled :: Int -> FilePath -> FilePath -> IO D.DataFrame
readSubsampled nRows src dst = do
    callCommand $ "head -n " ++ show nRows ++ " '" ++ src
        ++ "' | sed 's/ *$//' > '" ++ dst ++ "'"
    D.readSeparated feynmanOpts dst

computeR2 :: [Double] -> [Double] -> Double
computeR2 actual predicted =
    let n = fromIntegral (length actual) :: Double
        mean_y = sum actual / n
        ss_tot = sum [(y - mean_y) ^ (2 :: Int) | y <- actual]
        ss_res = sum [(y - yp) ^ (2 :: Int) | (y, yp) <- zip actual predicted]
    in if ss_tot == 0 then 1.0 else 1.0 - ss_res / ss_tot

data Result = Result
    { rName    :: String
    , rNVars   :: Int
    , rNModels :: Int
    , rTime    :: Double
    , rR2      :: Double
    , rFormula :: String
    } deriving Show

runOne :: Int -> Int -> Int -> Int -> Bool -> FilePath -> FilePath
       -> IO Result
runOne gens pop maxSz seed useEGraph src tmp = do
    df <- readSubsampled 500 src tmp
    let cols = D.columnNames df
        nVars = length cols - 1
        target = last cols
        cfg = benchConfig gens pop maxSz useEGraph
    t0 <- getCurrentTime
    models <- fit (mkStdGen seed) cfg (F.col target) df
    t1 <- getCurrentTime
    let elapsed = realToFrac (diffUTCTime t1 t0) :: Double
        best = last models  -- most complex = best fit
        predicted = D.columnAsList best (D.derive "pred" best df)
        actual = D.columnAsList (F.col target) df
        r2 = computeR2 actual predicted
        formula = D.prettyPrint best
    _ <- evaluate r2  -- force
    return $ Result "" nVars (length models) elapsed r2 formula

writeMarkdown :: FilePath -> [Result] -> Double -> IO ()
writeMarkdown path results totalTime =
    withFile path WriteMode $ \h -> do
        hPutStrLn h "# Feynman Benchmark Results"
        hPutStrLn h ""
        hPrintf h "**Config:** 50 generations, 100 population, max size 7, seed 42, 500 rows/dataset\n"
        hPrintf h "**Total time:** %.1fs | **Avg:** %.2fs/problem\n" totalTime
            (totalTime / fromIntegral (length results))
        hPutStrLn h ""
        hPutStrLn h "| # | Dataset | Vars | Models | R² | Time (s) | Best Formula |"
        hPutStrLn h "|---|---------|------|--------|----|----------|--------------|"
        mapM_ (\(i, r) ->
            hPrintf h "| %d | %s | %d | %d | %.4f | %.2f | `%s` |\n"
                (i :: Int) (rName r) (rNVars r) (rNModels r) (rR2 r) (rTime r) (rFormula r)
            ) (zip [1..] results)
        hPutStrLn h ""
        let goodR2 = length [r | r <- results, rR2 r >= 0.99]
            okR2   = length [r | r <- results, rR2 r >= 0.90, rR2 r < 0.99]
            poorR2 = length [r | r <- results, rR2 r < 0.90]
        hPutStrLn h "## Summary"
        hPutStrLn h ""
        hPrintf h "- R² >= 0.99: **%d** / %d\n" goodR2 (length results)
        hPrintf h "- R² >= 0.90: **%d** / %d\n" (goodR2 + okR2) (length results)
        hPrintf h "- R² < 0.90: **%d** / %d\n" poorR2 (length results)

main :: IO ()
main = do
    args <- getArgs
    let noEGraph = "--no-egraph" `elem` args
        args' = filter (/= "--no-egraph") args
        (feynmanDir, gens, pop, maxSz, seed) = case args' of
          [d,g,p,m,s] -> (d, read g, read p, read m, read s)
          [d]         -> (d, 50, 100, 7, 42)
          _           -> error "Usage: feynman-bench <feynman_dir> [gens pop maxsz seed] [--no-egraph]"

    files <- sort . filter (not . isPrefixOf ".") <$> listDirectory feynmanDir
    let nFiles = length files
        tmp = "/tmp/sr_feynman_tmp.dat"
        outMd = "bench/feynman_results.md"

    printf "%-20s %5s %6s %8s %8s\n"
        ("Dataset" :: String) ("Vars" :: String) ("Mod" :: String) ("R²" :: String) ("Time" :: String)
    printf "%s\n" (replicate 52 '-')

    totalT0 <- getCurrentTime
    results <- mapM (\(i, f) -> do
        let path = feynmanDir ++ "/" ++ f
        result <- try (runOne gens pop maxSz seed (not noEGraph) path tmp)
                    :: IO (Either SomeException Result)
        case result of
            Right r -> do
                let r' = r { rName = f }
                printf "%-20s %5d %6d %8.4f %7.2fs  [%d/%d]\n"
                    f (rNVars r') (rNModels r') (rR2 r') (rTime r') i nFiles
                hFlush stdout
                return (Just r')
            Left e -> do
                printf "%-20s %5s %6s %8s %8s  [%d/%d] %s\n"
                    f ("" :: String) ("ERR" :: String) ("" :: String) ("" :: String) i nFiles
                    (take 60 (show e))
                hFlush stdout
                return Nothing
        ) (zip [1..] files)

    totalT1 <- getCurrentTime
    let totalElapsed = realToFrac (diffUTCTime totalT1 totalT0) :: Double
        good = [r | Just r <- results]

    writeMarkdown outMd good totalElapsed
    printf "\n%d/%d succeeded | total %.1fs | avg %.2fs/problem\n"
        (length good) nFiles totalElapsed (totalElapsed / fromIntegral (max 1 (length good)))
    printf "Results written to %s\n" outMd
