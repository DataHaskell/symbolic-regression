{-# LANGUAGE OverloadedStrings #-}

module Example where

import qualified Data.Text as T
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Symbolic.Regression

example_predictMPG :: IO ()
example_predictMPG = do
    df <- D.readParquet "./data/mtcars.parquet"

    -- Define mpg as a column reference
    let mpg = F.col "mpg"

    exprs <- fit defaultRegressionConfig mpg df

    -- Print each expression
    mapM_
        (\(i, e) -> putStrLn $ "Model " ++ show i ++ ": " ++ D.prettyPrint e)
        (zip [1 ..] exprs)

    -- Create named expressions for different complexity levels
    let levels = zipWith (F..=) (map (T.pack . ("level_" ++) . show) [1 ..]) exprs

    -- deriveMany returns a DataFrame, so use 'let' not '<-'
    let df' = D.deriveMany levels df

    -- Derive prediction using the best (last) expression
    let df'' = D.derive "prediction" (last exprs) df'

    D.display (D.DisplayOptions 5) df''
