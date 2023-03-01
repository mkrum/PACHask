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

pacEvaluate :: PACTuple x Bool -> Int -> IO Float
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

randomIntervalFn :: Distribution (Float -> Bool) 
randomIntervalFn = do
    valOne <- getRandom
    valTwo <- getRandom
    if valOne < valTwo
        then return (isInInterval (Interval valOne valTwo))
        else return (isInInterval (Interval valTwo valOne))

data Point = Point {xValue :: Float, yValue :: Float}

data BoundingBox = BoundingBox { lowerCorner::Point, upperCorner::Point}

randomBoxFn :: Distribution (Point -> Bool)
randomBoxFn = do 
    (Point x1 y1) <- randomPoint
    (Point x2 y2) <- randomPoint 
    let (xmin, xmax) = if x1 <= x2 then (x1, x2) else (x2, x1)
    let (ymin, ymax) = if y1 <= y2 then (y1, y2) else (y2, y1)
    return (isInBox (BoundingBox (Point xmin ymin) (Point xmax ymax)))

randomPoint :: Distribution Point
randomPoint = do
    valOne <- getRandom
    valTwo <- getRandom
    return (Point valOne valTwo)

isInBox :: BoundingBox -> Point -> Bool
isInBox (BoundingBox (Point x1 y1) (Point x2 y2)) (Point inputX inputY) = and [(inputX >= x1), (inputX <= x2), (inputY >= y1), (inputY <= y2)]

learnBoundingBox :: [(Point, Bool)] -> Concept Point Bool
learnBoundingBox dataList =
      let positive_examples = filter (\x -> snd x) dataList
          positive_points = map (\x -> fst x) positive_examples
      in if (length positive_points) > 0
          then let xValues = map xValue positive_points
                   yValues = map yValue positive_points
                   lowerBounds = (Point (minimum xValues) (minimum yValues))
                   upperBounds = (Point (maximum xValues) (maximum yValues))
                in (isInBox (BoundingBox lowerBounds upperBounds))
          else (\x -> False)

main = do

  let intervalPAC = (getRandom, randomIntervalFn, learnInterval) :: PACTuple Float Bool
  let intervalBox = (randomPoint, randomBoxFn, learnBoundingBox) :: PACTuple Point Bool
  val <- pacEvaluate intervalBox 10

  putStrLn (show val)
