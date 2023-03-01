import System.Random
import Control.Monad.Random
import qualified Data.Set as Set

type Concept x y = x -> y
type Distribution x = Rand StdGen x

type PACTuple x y = (Distribution x, Distribution (Concept x y), [(x, y)] -> Concept x y)

labelData :: Concept x y -> [x] -> [(x, y)]
labelData concept dataList = map (\x -> (x, concept x)) dataList

isCorrect :: (Eq y) => Concept x y -> (x, y) -> Bool
isCorrect c (x, y) = (c x) == y

isIncorrect :: (Eq y) => Concept x y -> (x, y) -> Bool
isIncorrect = ((not .) . isCorrect)

errorOf :: (Eq y) => Concept x y -> [(x, y)] -> Float
errorOf concept dataList = 
  let evalList = map (\x -> if isIncorrect concept x then 1.0 else 0.0) dataList
      total = (fromIntegral . length) evalList
  in (sum evalList) / total

generateDataset :: (RandomGen g) => Int -> Rand g x -> Rand g [x]
generateDataset num valGenerator = sequence (replicate num valGenerator)

pacEvaluate :: PACTuple Float Bool -> Int -> IO Float
pacEvaluate (valGenerator, generateConcept, learnFn) numTrain = do

  hiddenConcept <- evalRandIO (generateConcept)
  trainPoints <- evalRandIO (generateDataset numTrain valGenerator)
  testPoints <- evalRandIO (generateDataset 10000 valGenerator)

  let labeledTrainPoints = labelData hiddenConcept trainPoints
  let labeledTestPoints = labelData hiddenConcept testPoints

  let learnedConcept = learnFn labeledTrainPoints 

  let epsilon = errorOf learnedConcept labeledTestPoints
  return epsilon

data Interval = Interval { lower::Float, upper::Float }

isInInterval :: Interval -> Float -> Bool
isInInterval (Interval lower upper) val = (val >= lower) && (val <= upper)

learnInterval :: [(Float, Bool)] -> Concept Float Bool
learnInterval dataList = 
    let positive_examples = filter (\x -> snd x) dataList
        positive_points = map (\x -> fst x) positive_examples
    in if (length positive_points) > 0
        then isInInterval (Interval (minimum positive_points) (maximum positive_points) )
        else (\x -> False)

randomIntervalFn :: (RandomGen g) => Rand g (Float -> Bool) 
randomIntervalFn = do
    valOne <- getRandom
    valTwo <- getRandom
    if valOne < valTwo
        then return (isInInterval (Interval valOne valTwo))
        else return (isInInterval (Interval valTwo valOne))

main = do

  let intervalPAC = (getRandom, randomIntervalFn, learnInterval) :: PACTuple Float Bool
  val <- pacEvaluate intervalPAC 10

  putStrLn (show val)
