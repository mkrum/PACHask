
module PAC where 

import Control.Monad.Random

type Concept x y = x -> y

type Distribution x = Rand StdGen x
sampleFrom :: Int -> Distribution x -> Distribution [x]
sampleFrom m dist = sequence (replicate m dist)

labelData :: Concept x y -> [x] -> [(x, y)]
labelData concept dataList = map (\x -> (x, concept x)) dataList

isIncorrect :: (Eq y) => Concept x y -> (x, y) -> Bool
isIncorrect c (x, y) = (c x) /= y

errorOf :: (Eq y) => Concept x y -> [(x, y)] -> Float
errorOf concept dataList = 
    let evalList = map (\x -> if isIncorrect concept x then 1.0 else 0.0) dataList
        total = (fromIntegral . length) evalList
     in (sum evalList) / total


type PACTuple x y = (Distribution x, Distribution (Concept x y), [(x, y)] -> Concept x y, Float)

pacEvaluate :: PACTuple x Bool -> Int -> IO Bool
pacEvaluate (distribution, generateConcept, learnFn, epsilon) m = do
  -- Sample a hidden concept, c
  hiddenConcept <- evalRandIO (generateConcept)
  -- Create a set of training points, S
  trainPoints <- evalRandIO (sampleFrom m distribution)
  -- Create an evaluation set to estimate R(h_S)
  testPoints <- evalRandIO (sampleFrom 10000 distribution)
  
  let labeledTrainPoints = labelData hiddenConcept trainPoints
      labeledTestPoints = labelData hiddenConcept testPoints
      -- Learn h_S 
      hypothesis = learnFn labeledTrainPoints
      -- Estimate its error
      measuredError = errorOf hypothesis labeledTestPoints
      -- Check whether this error is less than the desired bound
      success = measuredError <= epsilon

  return success

estimateDelta :: PACTuple x Bool -> Int -> Int -> IO Float
estimateDelta pac m n = do
  val <- sequence [pacEvaluate pac m | x <- [1..n]]
  let failures = map (\x -> if x then 0.0 else 1.0) val
  let mean = (sum failures) / (fromIntegral n)
  return mean

isInInterval :: Float -> Float -> Float -> Bool
isInInterval lower upper val = (val >= lower) && (val <= upper)

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

getPositivePoints :: [(x, Bool)] -> [x]
getPositivePoints = (map fst) . (filter snd)

pointsToInterval :: [Float] -> Concept Float Bool
pointsToInterval [] = \x -> False 
pointsToInterval positive_points = 
        isInInterval (minimum positive_points) (maximum positive_points)

learnInterval :: [(Float, Bool)] -> Concept Float Bool
learnInterval = (pointsToInterval . getPositivePoints)

outputData :: PACTuple a Bool -> Int -> Int -> Int -> IO ()
outputData pactuple n step max = do
    -- Create the range of potential m values, the size of the training set
    let mVals = [0,step..max]
    -- For each m, estimate the delta with n samples
    val <- sequence $ [ estimateDelta pactuple m n | m <- mVals]
    -- Format the string for output
    let both = zip val mVals
        fmtString currentString (val, m) = 
                currentString ++ (show m) ++ " " ++ (show val) ++ "\n"
        outputString = foldl fmtString "" both
    putStrLn outputString

type Point = (Float, Float)

boxInterval :: Concept Float Bool -> Concept Float Bool -> Concept Point Bool
boxInterval xInterval yInterval = \(x,y) -> ((xInterval x) && (yInterval y))

randomBox :: Distribution (Point -> Bool)
randomBox = do 
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

learnBox :: [(Point, Bool)] -> Concept Point Bool
learnBox = (pointsToBox . getPositivePoints)

type BoolVector = [Bool]
type LiteralVector = [Literal]

data Literal = Used | Negated | Unused
             deriving (Eq, Show)

evalLiteral :: Literal -> Bool -> Bool
evalLiteral Unused _  = True
evalLiteral Negated x = not x
evalLiteral Used x    = x

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

randomLiteralExpression :: Int -> Distribution (Concept BoolVector Bool)
randomLiteralExpression n = do
        random_val <- (sampleFrom n randomLiteral) 
        return (satisfiesLiteral random_val)

randomBoolVector :: Int -> Distribution BoolVector
randomBoolVector n = sampleFrom n getRandom

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

learnLiteralExpression :: [(BoolVector, Bool)] -> Concept BoolVector Bool
learnLiteralExpression = (pointsToBoolVector . getPositivePoints)
