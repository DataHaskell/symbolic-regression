{-# LANGUAGE OverloadedStrings #-}

module Example where

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Symbolic.Regression

main :: IO ()
main = do
    df <- D.readParquet "./data/mtcars.parquet"

    -- Define mpg as a column reference
    let mpg = F.col "mpg"

    exprs <- fit defaultRegressionConfig mpg df

    -- Print each expression (use show or custom formatting)
    mapM_ (putStrLn . show) exprs

    -- Create named expressions for different complexity levels
    let levels = zipWith (F..=) ["level_1", "level_2", "level_3"] exprs

    -- deriveMany returns a DataFrame, so use 'let' not '<-'
    let df' = D.deriveMany levels df

    -- Derive prediction using the best (last) expression
    let df'' = D.derive "prediction" (last exprs) df'

    -- Do something with the result
    pure ()