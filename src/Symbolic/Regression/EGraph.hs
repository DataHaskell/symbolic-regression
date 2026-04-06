{-# LANGUAGE ScopedTypeVariables #-}

-- | E-graph adapter wrapping hegg's API for symbolic regression.
module Symbolic.Regression.EGraph (
    -- * E-graph state
    SREGraph,
    emptySREGraph,

    -- * Core operations
    insertTree,
    insertTreeBinary,
    canonical,
    getBestExpr,
    mergeClasses,
    rebuildEGraph,

    -- * Saturation
    runSaturation,
    runSaturationN,

    -- * Queries
    eGraphClassCount,
    eGraphNodeCount,

    -- * Metadata (fitness, size, theta)
    getFitness,
    getSize,
    getTheta,
    insertFitness,
    getTopFitWithSize,
    getTopFit,
    getAllClassIds,
    doesExist,

    -- * Re-exports
    ClassId,
) where

import Control.Monad.State.Strict (runState, state)
import qualified Data.IntMap.Strict as IM
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as S

import Data.Equality.Extraction (extractBest)
import Data.Equality.Graph (
    ClassId,
    EGraph,
    addWithNorm,
    emptyEGraph,
    find,
    merge,
    rebuild,
 )
import Data.Equality.Graph.Classes (EClass (..), eClassNodes)
import Data.Equality.Graph.Internal (EGraph (..))
import Data.Equality.Graph.Monad (runEGraphM)
import Data.Equality.Graph.Nodes (ENode (..))
import Data.Equality.Saturation (runEqualitySaturationN)
import Data.Equality.Saturation.Scheduler (defaultBackoffScheduler)

import Symbolic.Regression.Expr
import Symbolic.Regression.Language (
    SRAnalysis (..),
    exprNormalize,
    exprNormalizeBinary,
    srCost,
 )
import Symbolic.Regression.Rewrites (srRewrites)

-- | The e-graph type for symbolic regression.
type SREGraph = EGraph SRAnalysis ExprF

emptySREGraph :: SREGraph
emptySREGraph = emptyEGraph

------------------------------------------------------------------------
-- Core operations
------------------------------------------------------------------------

-- | Insert a tree using context-aware normalization (Sub/Div elimination, AC flattening).
insertTree :: Fix ExprF -> SREGraph -> (ClassId, SREGraph)
insertTree (Fix node) eg =
    -- Recursively insert children first, threading e-graph state
    let (node', eg') = runState (traverse (state . insertTree) node) eg
     in addWithNorm exprNormalize (Node node') eg'

-- | Insert a tree using binary-only normalization (no SumF/ProdF containers).
insertTreeBinary :: Fix ExprF -> SREGraph -> (ClassId, SREGraph)
insertTreeBinary (Fix node) eg =
    let (node', eg') = runState (traverse (state . insertTreeBinary) node) eg
     in addWithNorm exprNormalizeBinary (Node node') eg'

canonical :: ClassId -> SREGraph -> ClassId
canonical = find

getBestExpr :: ClassId -> SREGraph -> Fix ExprF
getBestExpr cid eg = extractBest eg srCost (find cid eg)

mergeClasses :: ClassId -> ClassId -> SREGraph -> (ClassId, SREGraph)
mergeClasses = merge

rebuildEGraph :: SREGraph -> SREGraph
rebuildEGraph = rebuild

runSaturation :: SREGraph -> SREGraph
runSaturation eg =
    snd $
        runEGraphM eg (runEqualitySaturationN 1 defaultBackoffScheduler srRewrites)

runSaturationN :: Int -> SREGraph -> SREGraph
runSaturationN n eg =
    snd $
        runEGraphM eg (runEqualitySaturationN n defaultBackoffScheduler srRewrites)

------------------------------------------------------------------------
-- Queries
------------------------------------------------------------------------

eGraphClassCount :: SREGraph -> Int
eGraphClassCount = IM.size . classes

eGraphNodeCount :: SREGraph -> Int
eGraphNodeCount eg = sum [S.size (eClassNodes c) | c <- IM.elems (classes eg)]

------------------------------------------------------------------------
-- Metadata operations (fitness, size, theta)
------------------------------------------------------------------------

getFitness :: ClassId -> SREGraph -> Maybe Double
getFitness cid eg =
    let cid' = find cid eg
     in case IM.lookup cid' (classes eg) of
            Just ec -> srFitness (eClassData ec)
            Nothing -> Nothing

getSize :: ClassId -> SREGraph -> Int
getSize cid eg =
    let cid' = find cid eg
     in case IM.lookup cid' (classes eg) of
            Just ec -> srSize (eClassData ec)
            Nothing -> 0

getTheta :: ClassId -> SREGraph -> Maybe [PVector]
getTheta cid eg =
    let cid' = find cid eg
     in case IM.lookup cid' (classes eg) of
            Just ec -> srTheta (eClassData ec)
            Nothing -> Nothing

-- | Set fitness and theta for an e-class by modifying its analysis data.
insertFitness :: ClassId -> Double -> [PVector] -> SREGraph -> SREGraph
insertFitness cid fit theta eg =
    let cid' = find cid eg
     in eg{classes = IM.adjust updateClass cid' (classes eg)}
  where
    updateClass ec =
        ec
            { eClassData =
                (eClassData ec)
                    { srFitness = Just fit
                    , srTheta = Just theta
                    }
            }

-- | Top-k e-classes with a given size, sorted by fitness (best first).
getTopFitWithSize :: Int -> Int -> SREGraph -> [(ClassId, Double)]
getTopFitWithSize sz k eg =
    take k . sortBy (comparing (Down . snd)) $
        mapMaybe extract (IM.toList (classes eg))
  where
    extract (cid, ec) =
        let d = eClassData ec
         in if srSize d == sz
                then case srFitness d of
                    Just f -> Just (cid, f)
                    Nothing -> Nothing
                else Nothing

-- | Top-k e-classes overall, sorted by fitness (best first).
getTopFit :: Int -> SREGraph -> [(ClassId, Double)]
getTopFit k eg =
    take k . sortBy (comparing (Down . snd)) $
        mapMaybe extract (IM.toList (classes eg))
  where
    extract (cid, ec) = case srFitness (eClassData ec) of
        Just f -> Just (cid, f)
        Nothing -> Nothing

-- | All canonical class IDs.
getAllClassIds :: SREGraph -> [ClassId]
getAllClassIds = IM.keys . classes

-- | Check whether an e-node exists in the e-graph.
doesExist :: ExprF ClassId -> SREGraph -> Bool
doesExist node eg =
    let enode = Node (fmap (`find` eg) node)
     in any (S.member enode . eClassNodes) (IM.elems (classes eg))
