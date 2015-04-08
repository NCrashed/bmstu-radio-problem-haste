module Radio.Genetic where

import Prelude as P
import Data.Function
import Data.Functor
import Data.Maybe
import Data.List as List
import Control.Arrow 
import Control.Monad.Random as Rand
import Control.Monad as Monad
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe  
import Haste.Prim
import Haste.Foreign
import Haste 

import Radio.Task
import Radio.Tower 
import Radio.Random

type Chromosome = [Bool]
type Population = [Chromosome]
type GenRand = RandT HasteGen IO 
type Field = [[Int]]

data GeneticState = GeneticState {
  geneticFinished :: Bool,
  geneticCurrentGen :: Int,
  geneticPopulations :: [Population],
  geneticCurrentBest :: (Float, Chromosome),
  geneticGen :: HasteGen
} deriving (Show)

initialGeneticState :: IO GeneticState
initialGeneticState = GeneticState False 0 [] (0, []) <$> newHasteGen

extractSolution :: Input -> GeneticState -> Output
extractSolution input state = Output (filterTowers chr $ inputTowers input) fit 
  where (fit, chr) = findBest input $ geneticPopulations state

isGeneticFinished :: GeneticState -> Bool 
isGeneticFinished = geneticFinished

-- | Solving the problem using genetic algorithm
solve :: Input -> GeneticState -> IO GeneticState
solve input state 
  | isGeneticFinished state = return state
  | otherwise = do
    (newState, newGen) <- runRandT solve' $ geneticGen state
    return $ newState { geneticGen = newGen }
    where
      opts = inputEvolOptions input
      twrs = inputTowers input
      currGeneration = geneticCurrentGen state
      solve' = do
        pops <- if currGeneration == 0 
          then replicateM (popCount opts) $ initPopulation (indCount opts) (length twrs)
          else return $ geneticPopulations state
        newPops <- mapM (nextPopulation input) pops
        let currBest = findBest input newPops
        return $ state {
          geneticFinished = isFinished $ fst currBest,
          geneticCurrentGen = currGeneration + 1,
          geneticPopulations = newPops,
          geneticCurrentBest = currBest
        }

      isFinished fit = 
           currGeneration + 1 >= maxGeneration opts
        || (isJust (targetFitness opts) && fit >= fromJust (targetFitness opts))

-- | Calculates fitness using user function
fitness :: Input -> Chromosome -> Float
fitness input chr = unsafePerformIO $ userFunc coverage towerUsed towerCount
  where
    coverage = calcCoverage input chr
    towerUsed = length $ filterTowers chr $ inputTowers input
    towerCount = length $ inputTowers input

    userFunc :: Float -> Int -> Int -> IO Float
    userFunc = ffi $ toJSStr $ "(" ++ inputFitness input ++ ")"

-- | Calculates coverage of field by a solution
calcCoverage :: Input -> Chromosome -> Float 
calcCoverage input = calcCoverage' input . flip filterTowers towers
  where towers = inputTowers input 

calcCoverage' :: Input -> [Tower] -> Float 
calcCoverage' input = coverage . solutionField input
  where
    toFloat :: Int -> Float
    toFloat = fromIntegral . toInteger

    coverage :: Field -> Float
    coverage f = toFloat covered / toFloat area
      where
        covered = foldl' (\i b -> if b > 0 then i+1 else i) 0 $ concat f
        area = length $ concat f

-- | Builds field, each cell contains count of towers that covers the cell
solutionField :: Input -> [Tower] -> Field 
solutionField input towers = makeRow <$> [0 .. height]
  where
    (width, height) = inputFieldSize input
    makeRow y = (\x -> sum $ bool2int . inRadius x y <$> towers) <$> [0 .. width]

    bool2int b = if b then 1 else 0

    inRadius :: Int -> Int -> Tower -> Bool
    inRadius x y t = dx*dx + dy*dy <= r*r
      where 
        r = fromIntegral (towerRadius t) + 0.5
        dx = fromIntegral $ towerX t - x 
        dy = fromIntegral $ towerY t - y

-- | Returns only placed towers by solution in chromosome
filterTowers :: Chromosome -> [Tower] -> [Tower]
filterTowers chr = fmap snd . filter ((== True) . fst) . zip chr

-- | Fetching best solution from populations      
findBest :: Input -> [Population] -> (Float, Chromosome)
findBest input pops = maximumBy (compare `on` fst) $ findPopBest input <$> pops

-- | Fetching best solution from population
findPopBest :: Input -> Population -> (Float, Chromosome)
findPopBest input pop = maximumBy (compare `on` fst) $ first (fitness input) <$> zip pop pop
    
-- | Creating chromosome with random values, n is a length of chromosome
initChromosome :: Int -> GenRand Chromosome
initChromosome n = replicateM n getRandom

-- | Creating population with m chromosomes with length n
initPopulation :: Int -> Int -> GenRand Population
initPopulation m n = replicateM m $ initChromosome n

-- | Helper to choose between two elements with provided chance of the first one
randChoice :: Rational -> GenRand a -> GenRand a -> GenRand a
randChoice chance th els = join (Rand.fromList [(th, chance), (els, 1 - chance)])

-- | Caclulates next generation of population
nextPopulation :: Input -> Population -> GenRand Population
nextPopulation input pop = do 
  newPop' <- liftM concat $ replicateM (ceiling $ fromIntegral nonEliteCount / 2) $ do
    a1 <- takeChr
    b1 <- takeChr
    (a2, b2) <- crossover a1 b1
    a3 <- applyMutation a2
    b3 <- applyMutation b2
    return [a3, b3]
  let newPop = elite ++ newPop'
  return $ if length newPop <= length pop then newPop else init newPop
  where opts = inputEvolOptions input
        fits = toRational <$> fitness input <$> pop
        maxfit = maximum fits
        chances = zip pop ((/maxfit) <$> fits)
        takeChr = Rand.fromList chances
        mutChance = toRational $ mutationChance opts
        applyMutation c = randChoice mutChance (mutation c) (return c)
        bests = snd <$> sortBy (flip compare `on` fst) (first (fitness input) <$> zip pop pop)
        elite = take (ceiling $ fromIntegral (length bests) * elitePart opts) bests
        nonEliteCount = length pop - length elite
        
-- | Crossover operator, fallbacks to trival cases if length isn't enough for
-- thre pointed crossover
crossover :: Chromosome -> Chromosome -> GenRand (Chromosome, Chromosome)
crossover a b
  | length a /= length b = error "Chromosomes have different lengths"
  | length a <= 1 = return (a, b)
  | length a == 2 = 
    let [a0, a1] = a 
        [b0, b1] = b
    in return ([a0, b1], [b0, a1])
  | otherwise = crossover3 a b
  
-- | Implements three point crossover. Length of chromosome must be > 3
crossover3 :: Chromosome -> Chromosome -> GenRand (Chromosome, Chromosome)
crossover3 a b = do
  [p1, p2, p3] <- sort <$> replicateM 3 (getRandomR (1, n - 2))
  let a' = concat [slice 0 p1 a, slice p1 p2 b, slice p2 p3 a, slice p3 n b]
  let b' = concat [slice 0 p1 b, slice p1 p2 a, slice p2 p3 b, slice p3 n a]
  return $ (a', b')
  where
    n = length a
    slice i1 i2 = take (i2 - i1) . drop i1

-- | Implements mutation of one bit
mutation :: Chromosome -> GenRand Chromosome
mutation [] = return []
mutation a = do
  i <- getRandomR (0, length a - 1)
  let pre = take i a 
      post = drop (i + 1) a 
  return $ pre ++ [not $ a !! i] ++ post