
import System.Random
import Control.Monad.Random

type Concept x y = x -> y
type Distribution x = Rand StdGen x

sampleFrom :: Int -> Distribution x -> Distribution [x]
sampleFrom m dist = sequence (replicate m dist)

type PACTuple x y = (Distribution x, Distribution (Concept x y), [(x, y)] -> Concept x y, Float)

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

getPositivePoints :: [(x, Bool)] -> [x]
getPositivePoints = (map fst) . (filter snd)

pacEvaluate :: PACTuple x Bool -> Int -> IO Bool
pacEvaluate (distribution, generateConcept, learnFn, epsilon) m = do

  hiddenConcept <- evalRandIO (generateConcept)
  trainPoints <- evalRandIO (sampleFrom m distribution)
  testPoints <- evalRandIO (sampleFrom 10000 distribution)

  let labeledTrainPoints = labelData hiddenConcept trainPoints
      labeledTestPoints = labelData hiddenConcept testPoints
      measuredError = errorOf (learnFn labeledTrainPoints) labeledTestPoints
      success = measuredError <= epsilon
  return success

estimateDelta :: PACTuple x Bool -> Int -> Int -> IO Float
estimateDelta pac m n = do
  val <- sequence [pacEvaluate pac m | x <- [1..n]]
  let succcess = map (\x -> if x then 1.0 else 0.0) val
  let mean = (sum succcess) / (fromIntegral n)
  return mean

isInInterval :: Float -> Float -> Float -> Bool
isInInterval lower upper val = (val >= lower) && (val <= upper)

pointsToInterval :: [Float] -> Concept Float Bool
pointsToInterval [] = \x -> False 
pointsToInterval positive_points = 
        isInInterval (minimum positive_points) (maximum positive_points)

learnInterval :: [(Float, Bool)] -> Concept Float Bool
learnInterval = (pointsToInterval . getPositivePoints)

randomBounds :: Distribution (Float, Float)
randomBounds = do
    valOne <- getRandom
    valTwo <- getRandom
    if valOne < valTwo
        then return (valOne, valTwo)
        else return (valTwo, valOne)

randomInterval :: Distribution (Float -> Bool) 
randomInterval = do
    (lower, upper) <- randomBounds
    return (isInInterval lower upper)

type Point = (Float, Float)

boxInterval :: Concept Float Bool -> Concept Float Bool -> Concept Point Bool
boxInterval xInterval yInterval = \(x,y) -> ((xInterval x) && (yInterval y))

randomBoxFn :: Distribution (Point -> Bool)
randomBoxFn = do 
    xInterval <- randomInterval
    yInterval <- randomInterval
    return (boxInterval xInterval yInterval)

randomPoint :: Distribution Point
randomPoint = do
    valOne <- getRandom
    valTwo <- getRandom
    return (valOne, valTwo)

pointsToBox :: [Point] -> (Point -> Bool)
pointsToBox [] = \x -> False
pointsToBox positive_points = 
        let xInterval = pointsToInterval (map fst positive_points)
            yInterval = pointsToInterval (map snd positive_points)
        in boxInterval xInterval yInterval

learnBoundingBox :: [(Point, Bool)] -> Concept Point Bool
learnBoundingBox = (pointsToBox . getPositivePoints)

type BoolVector = [Bool]
type LiteralVector = [Literal]

data Literal = Used | Negated | Unused
             deriving (Eq, Show)

evalLiteral :: Literal -> Bool -> Bool
evalLiteral Unused _      = True
evalLiteral Negated True  = False
evalLiteral Negated False = True
evalLiteral Used x        = x

satisfiesLiteral :: LiteralVector -> BoolVector -> Bool
satisfiesLiteral [] [] = True
satisfiesLiteral l [] = False
satisfiesLiteral [] b = False
satisfiesLiteral (l:otherLiterals) (b:otherBools) = (evalLiteral l b) && satisfiesLiteral otherLiterals otherBools

floatToLiterval :: Float -> Literal
floatToLiterval val 
  | val <= 0.1 = Used
  | val <= 0.2 = Negated
  | otherwise = Unused

randomLiteral :: Distribution Literal
randomLiteral = do
    val <- getRandom
    return (floatToLiterval val)

randomBoolVector :: Int -> Distribution BoolVector
randomBoolVector n = sampleFrom n getRandom

randomBoolVectorFn :: Int -> Distribution (Concept BoolVector Bool)
randomBoolVectorFn n = do
        random_val <- (sampleFrom n randomLiteral) 
        return (satisfiesLiteral random_val)

updateLiteral :: Literal -> Bool -> Literal
updateLiteral Used True = Used
updateLiteral Used False = Negated
updateLiteral Negated True = Unused
updateLiteral Negated False = Negated
updateLiteral Unused _ = Unused

updateLiteralVector :: BoolVector -> LiteralVector -> LiteralVector
updateLiteralVector [] [] = []
updateLiteralVector l [] = []
updateLiteralVector [] b = []
updateLiteralVector (b:otherBools) (l:otherLiterals) = (updateLiteral l b):(updateLiteralVector otherBools otherLiterals)

pointsToBoolVector :: [BoolVector] -> Concept BoolVector Bool
pointsToBoolVector dataList = let
            newAssign = foldr (updateLiteralVector) (repeat Used) dataList
         in (satisfiesLiteral newAssign)

learnBoolVector :: [(BoolVector, Bool)] -> Concept BoolVector Bool
learnBoolVector = (pointsToBoolVector . getPositivePoints)

main = do

  let intervalPAC = (getRandom, randomInterval, learnInterval, 0.01) :: PACTuple Float Bool
  let boxPAC = (randomPoint, randomBoxFn, learnBoundingBox, 0.01) :: PACTuple Point Bool
  let boolPAC = 
            \n -> ((randomBoolVector n), (randomBoolVectorFn n), learnBoolVector, 0.01) 

  val <- estimateDelta (boolPAC 32) 100 10
  putStrLn (show val)
