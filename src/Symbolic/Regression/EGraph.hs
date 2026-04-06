{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
-- | E-graph adapter: wraps hegg's API to provide the interface that
-- Symbolic.Regression expects (previously provided by Algorithm.EqSat).
--
-- Uses hegg's 'normalizeNode' for A\/C canonicalization of Add\/Mul,
-- eliminating commutativity rewrite rules.
module Symbolic.Regression.EGraph
    ( -- * E-graph state
      SREGraph
    , RndEGraph
    , emptySREGraph

      -- * Core operations
    , insertTree
    , canonical
    , getBestExpr
    , mergeClasses
    , rebuildEGraph

      -- * Saturation
    , runSaturation

      -- * Queries
    , eGraphClassCount
    , eGraphNodeCount

      -- * Re-exports
    , ClassId
    ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

import qualified Data.Equality.Utils as H
import Data.Equality.Graph (EGraph, ClassId, Language(..), represent, find, merge, rebuild, emptyEGraph)
import Data.Equality.Graph.Internal (EGraph(..))
import Data.Equality.Graph.Classes (EClass(..))
import Data.Equality.Extraction (extractBest)
import Data.Equality.Saturation (runEqualitySaturationN)
import Data.Equality.Saturation.Scheduler (defaultBackoffScheduler)
import Data.Equality.Graph.Monad (runEGraphM)

import Data.SRTree.Internal (SRTree(..))
import qualified Data.SRTree.Recursion as SR

import Symbolic.Regression.Language (SRAnalysis(..), srCost, toHeggFix, fromHeggFix)
import Symbolic.Regression.Rewrites (srRewrites)

-- | The e-graph type for symbolic regression
type SREGraph = EGraph SRAnalysis SRTree

-- | Monad for e-graph operations with random state.
-- Unlike srtree's RndEGraph which is StateT EGraph (StateT StdGen IO),
-- we keep the e-graph as a plain value and thread it explicitly.
-- This type alias exists for documentation.
type RndEGraph = SREGraph

-- | Empty e-graph
emptySREGraph :: SREGraph
emptySREGraph = emptyEGraph

-- | Insert a tree into the e-graph, return its canonical class ID
insertTree :: SR.Fix SRTree -> SREGraph -> (ClassId, SREGraph)
insertTree tree eg =
    let (!cid, !eg') = represent (toHeggFix tree) eg
    in (find cid eg', eg')

-- | Get canonical class ID
canonical :: ClassId -> SREGraph -> ClassId
canonical = find

-- | Extract the best (lowest-cost) expression from an e-class
getBestExpr :: ClassId -> SREGraph -> SR.Fix SRTree
getBestExpr cid eg = fromHeggFix (extractBest eg srCost (find cid eg))

-- | Merge two e-classes
mergeClasses :: ClassId -> ClassId -> SREGraph -> (ClassId, SREGraph)
mergeClasses c1 c2 eg = merge c1 c2 eg

-- | Rebuild the e-graph (restore invariants after merges)
rebuildEGraph :: SREGraph -> SREGraph
rebuildEGraph = rebuild

-- | Run one iteration of equality saturation with algebraic rewrite rules.
-- Uses hegg's backoff scheduler. Commutativity is handled by
-- normalizeNode (not rewrite rules).
-- Matches symregg's approach: 1 iteration per call (not full 30-iteration saturation).
runSaturation :: SREGraph -> SREGraph
runSaturation eg =
    snd $ runEGraphM eg (runEqualitySaturationN 1 defaultBackoffScheduler srRewrites)

-- | Number of e-classes in the e-graph
eGraphClassCount :: SREGraph -> Int
eGraphClassCount = IM.size . classes

-- | Number of e-nodes in the e-graph
eGraphNodeCount :: SREGraph -> Int
eGraphNodeCount eg = sum [ S.size (eClassNodes c) | c <- IM.elems (classes eg) ]
