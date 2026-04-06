{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Genetic programming loop with hegg equality saturation.
module Symbolic.Regression.GP (
    GPConfig (..),
    defaultGPConfig,
    runGP,
) where

import Control.Monad (when)
import qualified Data.IntMap.Strict as IM
import Data.List (maximumBy, sortBy)
import Data.Ord (Down (..), comparing)
import System.Random

import Data.Equality.Extraction
import Data.Equality.Graph hiding (add)
import Data.Equality.Graph.Internal (EGraph (..))
import Data.Equality.Graph.Monad (runEGraphM)
import Data.Equality.Saturation (runEqualitySaturation)
import Data.Equality.Saturation.Scheduler

import Symbolic.Regression.Expr
import Symbolic.Regression.Expr.Utils (countNodes)
import Symbolic.Regression.Fitness
import Symbolic.Regression.Language (SRAnalysis, srCost)
import Symbolic.Regression.Rewrites (srRewrites)

-- | GP configuration
data GPConfig = GPConfig
    { cfgGenerations :: {-# UNPACK #-} !Int
    , cfgPopSize :: {-# UNPACK #-} !Int
    , cfgMaxSize :: {-# UNPACK #-} !Int
    , cfgTournSize :: {-# UNPACK #-} !Int
    , cfgCrossoverProb :: {-# UNPACK #-} !Double
    , cfgMutationProb :: {-# UNPACK #-} !Double
    , cfgSeed :: {-# UNPACK #-} !Int
    , cfgShowTrace :: !Bool
    , cfgNumFeatures :: {-# UNPACK #-} !Int
    }
    deriving (Show)

defaultGPConfig :: GPConfig
defaultGPConfig =
    GPConfig
        { cfgGenerations = 100
        , cfgPopSize = 100
        , cfgMaxSize = 7
        , cfgTournSize = 3
        , cfgCrossoverProb = 0.9
        , cfgMutationProb = 0.3
        , cfgSeed = 42
        , cfgShowTrace = True
        , cfgNumFeatures = 1
        }

-- | GP state threaded through the evolution loop.
data GPState = GPState
    { gsEGraph :: !(EGraph SRAnalysis ExprF)
    , gsFitness :: !(IM.IntMap Double)
    , gsExprs :: !(IM.IntMap (Fix ExprF))
    , gsPopIds :: ![ClassId]
    , gsGen :: !StdGen
    }

-- | Run GP, return Pareto front of (size, fitness, expression)
runGP :: GPConfig -> DataSet -> DataSet -> IO [(Int, Double, Fix ExprF)]
runGP cfg trainData _testData = do
    let g0 = mkStdGen (cfgSeed cfg)
        nVars = cfgNumFeatures cfg

    let (!trees, !g1) = genRandomTrees (cfgPopSize cfg) nVars (cfgMaxSize cfg) g0
        (!ids, !eg0) =
            foldl
                ( \(!acc, !eg) tree ->
                    let (!cid, !eg') = represent tree eg
                     in (cid : acc, eg')
                )
                ([], emptyEGraph :: EGraph SRAnalysis ExprF)
                trees
        st0 = GPState eg0 IM.empty IM.empty ids g1

    let !st1 = evaluateAll trainData st0

    stFinal <- go 0 st1
    return $ extractPareto stFinal
  where
    go !gen !st
        | gen >= cfgGenerations cfg = return st
        | otherwise = do
            let !st' = oneGen cfg trainData st
            when (cfgShowTrace cfg && gen `mod` 10 == 0) $
                putStrLn $
                    "Gen "
                        ++ show gen
                        ++ " | pop="
                        ++ show (length (gsPopIds st'))
                        ++ " | classes="
                        ++ show (IM.size (classes (gsEGraph st')))
                        ++ " | best="
                        ++ show (bestFit st')
            go (gen + 1) st'

oneGen :: GPConfig -> DataSet -> GPState -> GPState
oneGen cfg trainData st =
    let (!offspring, !g1) = createOffspring cfg st

        (!newIds, !eg1) =
            foldl
                ( \(!acc, !eg) tree ->
                    let (!cid, !eg') = represent tree eg
                     in (find cid eg' : acc, eg')
                )
                ([], gsEGraph st)
                offspring

        nClasses = IM.size (classes eg1)
        !eg2 =
            if nClasses < 1500
                then
                    snd $
                        runEGraphM
                            eg1
                            (runEqualitySaturation defaultBackoffScheduler srRewrites)
                else eg1

        !fitMap' =
            IM.foldlWithKey'
                ( \(!fm) k v ->
                    IM.insertWith max (find k eg2) v fm
                )
                IM.empty
                (gsFitness st)
        !exprMap' =
            IM.foldlWithKey'
                ( \(!em) k v ->
                    let k' = find k eg2
                     in if IM.member k' em then em else IM.insert k' v em
                )
                IM.empty
                (gsExprs st)

        allIds = map (`find` eg2) (gsPopIds st ++ newIds)
        st1 =
            st
                { gsEGraph = eg2
                , gsFitness = fitMap'
                , gsExprs = exprMap'
                , gsPopIds = allIds
                , gsGen = g1
                }

        !st2 = evaluateAll trainData st1
     in selectTopN cfg st2

evaluateAll :: DataSet -> GPState -> GPState
evaluateAll trainData st =
    let eg = gsEGraph st
        go (!fm, !em, !g) cid =
            let !cid' = find cid eg
             in if IM.member cid' fm
                    then (fm, em, g)
                    else
                        let !expr = extractBest eg srCost cid'
                            (!fit, _theta, !g') = evaluateFitness 30 2 trainData expr g
                         in (IM.insert cid' fit fm, IM.insert cid' expr em, g')
        (!fitMap, !exprMap, !gNew) = foldl go (gsFitness st, gsExprs st, gsGen st) (gsPopIds st)
     in st{gsFitness = fitMap, gsExprs = exprMap, gsGen = gNew}

createOffspring :: GPConfig -> GPState -> ([Fix ExprF], StdGen)
createOffspring cfg st = go (cfgPopSize cfg `div` 2) (gsGen st) []
  where
    go 0 !g !acc = (acc, g)
    go !n !g !acc =
        let (!p1, !g1) = tournament cfg st g
            (!p2, !g2) = tournament cfg st g1
            !e1 = lookupExpr p1 st
            !e2 = lookupExpr p2 st
            (!r, !g3) = randomR (0.0 :: Double, 1.0) g2
            (!child, !g4) =
                if r < cfgCrossoverProb cfg
                    then crossover e1 e2 g3
                    else (e1, g3)
            (!rm, !g5) = randomR (0.0 :: Double, 1.0) g4
            (!mutant, !g6) =
                if rm < cfgMutationProb cfg
                    then mutate (cfgNumFeatures cfg) child g5
                    else (child, g5)
         in go (n - 1) g6 (mutant : acc)

lookupExpr :: ClassId -> GPState -> Fix ExprF
lookupExpr cid st =
    let cid' = find cid (gsEGraph st)
     in case IM.lookup cid' (gsExprs st) of
            Just e -> e
            Nothing -> extractBest (gsEGraph st) srCost cid'

tournament :: GPConfig -> GPState -> StdGen -> (ClassId, StdGen)
tournament cfg st g =
    let pop = gsPopIds st
        eg = gsEGraph st
        (!candidates, !g') = pickN (cfgTournSize cfg) pop g
        scored =
            [ (cid, IM.findWithDefault (-1e18) (find cid eg) (gsFitness st))
            | cid <- candidates
            ]
     in (fst $ maximumBy (comparing snd) scored, g')

pickN :: Int -> [a] -> StdGen -> ([a], StdGen)
pickN 0 _ !g = ([], g)
pickN _ [] !g = ([], g)
pickN !n xs !g =
    let (!i, !g') = randomR (0, length xs - 1) g
        (!rest, !g'') = pickN (n - 1) xs g'
     in (xs !! i : rest, g'')

crossover :: Fix ExprF -> Fix ExprF -> StdGen -> (Fix ExprF, StdGen)
crossover t1 t2 !g =
    let !sz1 = countNodes t1
        !sz2 = countNodes t2
        (!i, !g1) = randomR (0, max 0 (sz1 - 1)) g
        (!j, !g2) = randomR (0, max 0 (sz2 - 1)) g1
        !donor = getSubAt j t2
     in (replaceSubAt i donor t1, g2)

mutate :: Int -> Fix ExprF -> StdGen -> (Fix ExprF, StdGen)
mutate nVars tree !g =
    let !sz = countNodes tree
        (!i, !g1) = randomR (0, max 0 (sz - 1)) g
        (!newSub, !g2) = buildRandom nVars 2 g1
     in (replaceSubAt i newSub tree, g2)

getSubAt :: Int -> Fix ExprF -> Fix ExprF
getSubAt 0 t = t
getSubAt n (Fix node) = case node of
    BinF _ l r ->
        let !lSz = countNodes l
         in if n - 1 < lSz then getSubAt (n - 1) l else getSubAt (n - 1 - lSz) r
    UnF _ c -> getSubAt (n - 1) c
    _ -> Fix node

replaceSubAt :: Int -> Fix ExprF -> Fix ExprF -> Fix ExprF
replaceSubAt 0 !replacement _ = replacement
replaceSubAt !n !replacement (Fix node) = case node of
    BinF op l r ->
        let !lSz = countNodes l
         in if n - 1 < lSz
                then Fix (BinF op (replaceSubAt (n - 1) replacement l) r)
                else Fix (BinF op l (replaceSubAt (n - 1 - lSz) replacement r))
    UnF f c -> Fix (UnF f (replaceSubAt (n - 1) replacement c))
    _ -> Fix node

selectTopN :: GPConfig -> GPState -> GPState
selectTopN cfg st =
    let sorted =
            take (cfgPopSize cfg)
                . sortBy (comparing (Down . snd))
                . IM.toList
                $ gsFitness st
     in st{gsPopIds = map fst sorted}

extractPareto :: GPState -> [(Int, Double, Fix ExprF)]
extractPareto st =
    let eg = gsEGraph st
        bySize =
            IM.foldlWithKey'
                ( \(!m) cid fit ->
                    let !expr = case IM.lookup cid (gsExprs st) of
                            Just e -> e
                            Nothing -> extractBest eg srCost cid
                        !sz = countNodes expr
                     in IM.insertWith
                            (\(f1, e1) (f2, e2) -> if f1 > f2 then (f1, e1) else (f2, e2))
                            sz
                            (fit, expr)
                            m
                )
                IM.empty
                (gsFitness st)
        candidates = sortBy (comparing fst) (IM.toList bySize)
     in buildPareto (-1e18) candidates

buildPareto ::
    Double -> [(Int, (Double, Fix ExprF))] -> [(Int, Double, Fix ExprF)]
buildPareto _ [] = []
buildPareto !best ((sz, (fit, expr)) : rest)
    | fit > best = (sz, fit, expr) : buildPareto fit rest
    | otherwise = buildPareto best rest

bestFit :: GPState -> Double
bestFit st = if IM.null (gsFitness st) then -1e18 else maximum (IM.elems (gsFitness st))

genRandomTrees :: Int -> Int -> Int -> StdGen -> ([Fix ExprF], StdGen)
genRandomTrees 0 _ _ !g = ([], g)
genRandomTrees !n nVars maxD !g =
    let (!d, !g1) = randomR (1, maxD) g
        (!tree, !g2) = buildRandom nVars d g1
        (!rest, !g3) = genRandomTrees (n - 1) nVars maxD g2
     in (tree : rest, g3)

buildRandom :: Int -> Int -> StdGen -> (Fix ExprF, StdGen)
buildRandom nVars 0 !g =
    let (!isVar, !g1) = random g
     in if (isVar :: Bool)
            then
                let (!vi, !g2) = randomR (0, max 0 (nVars - 1)) g1
                 in (Fix (VarF vi), g2)
            else
                let (!v, !g2) = randomR (-5.0 :: Double, 5.0) g1
                 in (Fix (LitF v), g2)
buildRandom nVars !depth !g =
    let (!r, !g1) = randomR (0 :: Int, 9) g
     in if r < 3
            then buildRandom nVars 0 g1
            else
                if r < 7
                    then
                        let ops = [Add, Sub, Mul, Div]
                            (!oi, !g2) = randomR (0, length ops - 1) g1
                            (!l, !g3) = buildRandom nVars (depth - 1) g2
                            (!r', !g4) = buildRandom nVars (depth - 1) g3
                         in (Fix (BinF (ops !! oi) l r'), g4)
                    else
                        let fns = [Sq, Log, Exp, Recip]
                            (!fi, !g2) = randomR (0, length fns - 1) g1
                            (!c, !g3) = buildRandom nVars (depth - 1) g2
                         in (Fix (UnF (fns !! fi) c), g3)
