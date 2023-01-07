{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import System.Random
import qualified Data.Set as Set

type Concept x y = x -> y

class BoolPACConcept a x where
    learn :: [(x, Bool)] -> a
    apply :: a -> Concept x Bool

labelData :: Concept x y -> [x] -> [(x, y)]
labelData concept dataList = map (\x -> (x, concept x)) dataList

isCorrect :: (Eq y) => Concept x y -> (x, y) -> Bool
isCorrect c (x, y) = (c x) == y

isIncorrect :: (Eq y) => Concept x y -> (x, y) -> Bool
isIncorrect = ((not .) . isCorrect)

errorOf :: (Eq y) => Concept x y -> [(x, y)] -> Float
errorOf c dataList = 
  let incorrectList = filter (\x -> isIncorrect c x) dataList
      total = (fromIntegral (length dataList))::Float
      totalIncorrect = (fromIntegral (length incorrectList))::Float
  in totalIncorrect / total

evaluateConcepts :: (Eq y) => Concept x y -> Concept x y -> (StdGen -> (x, StdGen)) -> StdGen -> (Float, StdGen)
evaluateConcepts concept hypothesis dataGen g = let
  (testPoints, g1) = generateDataset 10000 dataGen g
  labeledTestPoints = labelData concept testPoints
  in ((errorOf hypothesis labeledTestPoints), g1)

chainGenerate :: (StdGen -> (x, StdGen)) -> StdGen -> [(x, StdGen)]
chainGenerate genFun g = let (val, g1) = genFun g
                         in (val, g) : (chainGenerate genFun g1)

generateDataset :: Int -> (StdGen -> (x, StdGen)) -> StdGen -> ([x], StdGen)
generateDataset num valGenerator g = let
 samples = take num (chainGenerate valGenerator g)
 points = map fst samples
 g1 = last $ map snd samples
 in (points, g1)

pacEvaluate :: (BoolPACConcept a x) => ([(x, Bool)] -> a) -> (StdGen -> (a, StdGen)) -> (StdGen -> (x, StdGen)) -> Int -> StdGen -> (Float, StdGen) 
pacEvaluate learnFn generateConcept valGenerator numTrain g = let
  (hiddenConcept, g1) = generateConcept g 
  (trainPoints, g2) = generateDataset numTrain valGenerator g1 
  labeledTrainPoints = labelData (apply hiddenConcept) trainPoints 
  learnedConcept = learnFn labeledTrainPoints 
  in (evaluateConcepts (apply hiddenConcept) (apply learnedConcept) valGenerator g2)

data Interval = Interval { lower::Float, upper::Float }

isInInterval :: Interval -> Float -> Bool
isInInterval (Interval lower upper) val = (val >= lower) && (val <= upper)

instance BoolPACConcept Interval Float where

  apply = isInInterval

  learn dataList = 
        let positive_examples = filter (\x -> snd x) dataList
            positive_points = map (\x -> fst x) positive_examples
        in if (length positive_points) > 0
            then Interval (minimum positive_points) (maximum positive_points) 
            else Interval (fst (dataList !! 0)) (fst (dataList !! 0)) 

randomInterval :: StdGen -> (Interval, StdGen)
randomInterval g =
    let (valOne, g1) = random g
        (valTwo, g2) = random g1
   in if valOne < valTwo
      then (Interval valOne valTwo, g2)
      else (Interval valTwo valOne, g2)

data Point = Point {xValue :: Float, yValue :: Float}

data BoundingBox = BoundingBox { lowerCorner::Point, upperCorner::Point}

randomPoint :: StdGen -> (Point, StdGen)
randomPoint g = let (xVal, g1) = random g
                    (yVal, g2) = random g1
                in ((Point xVal yVal), g2)

isInBox :: BoundingBox -> Point -> Bool
isInBox (BoundingBox (Point x1 y1) (Point x2 y2)) (Point inputX inputY) = and [(inputX >= x1), (inputX <= x2), (inputY >= y1), (inputY <= y2)]

instance BoolPACConcept BoundingBox Point where

  apply = isInBox

  learn dataList = 
        let positive_examples = filter (\x -> snd x) dataList
            positive_points = map (\x -> fst x) positive_examples
        in if (length positive_points) > 0
            then let xValues = map xValue positive_points
                     yValues = map yValue positive_points
                     lowerBounds = (Point (minimum xValues) (minimum yValues))
                     upperBounds = (Point (maximum xValues) (maximum yValues))
                  in BoundingBox lowerBounds upperBounds 
            else BoundingBox (Point 1.0 1.0) (Point 1.0 1.0) 

randomBox :: StdGen -> (BoundingBox, StdGen)
randomBox g =
    let ((Point x1 y1), g1) = randomPoint g
        ((Point x2 y2), g2) = randomPoint g1
        (xmin, xmax) = if x1 <= x2 then (x1, x2) else (x2, x1)
        (ymin, ymax) = if y1 <= y2 then (y1, y2) else (y2, y1)
   in ((BoundingBox (Point xmin ymin) (Point xmax ymax)), g2)


type BoolVector = [Bool]
type LiteralVector = [Literal]

data Literal = Used | Negated | Unused
             deriving (Eq, Show)

evalLiteral :: Literal -> Bool -> Bool
evalLiteral Unused _ = True
evalLiteral Negated True = False
evalLiteral Negated False = True
evalLiteral Used x = x

satisfiesLiteral :: LiteralVector -> BoolVector -> Bool
satisfiesLiteral [] [] = True
satisfiesLiteral l [] = False
satisfiesLiteral [] b = False
satisfiesLiteral (l:otherLiterals) (b:otherBools) = (evalLiteral l b) && satisfiesLiteral otherLiterals otherBools

randomLiteral :: StdGen -> (Literal, StdGen)
randomLiteral g = let (val, g1) = random g :: (Float, StdGen)
                in case val of
                     val | val <= 0.1 -> (Used, g1)
                         | val <= 0.2 -> (Negated, g1)
                         | otherwise -> (Unused, g1)

randomLiteralVector :: Int -> StdGen -> (LiteralVector, StdGen)
randomLiteralVector n = generateDataset n randomLiteral

randomBool :: StdGen -> (Bool, StdGen)
randomBool g = let (val, g1) = random g :: (Float, StdGen)
                in (val <= 0.5, g1)

randomBoolVector :: Int -> StdGen -> (BoolVector, StdGen)
randomBoolVector n = generateDataset n randomBool

instance BoolPACConcept LiteralVector BoolVector where

  apply = satisfiesLiteral

  learn dataList = 
          let n = length (fst $ dataList !! 0)
              positive_examples = filter (\x -> snd x) dataList
              positive_points = map (\x -> fst x) positive_examples
        in if (length positive_points) > 0
            then let assignments = [ Set.fromList $ map (!! i) positive_points | i <- [0..n-1]]
                     getAssignment assign = case assign of
                                        assign | (length assign) == 2 -> Unused
                                               | True `elem` assign -> Used
                                               | otherwise -> Negated
                  in map getAssignment assignments
        else [Used | i <- [1..n]]

main = do
  g <- getStdGen
  --let randFn = randomInterval :: StdGen -> (Interval, StdGen)
  --let learnFn = learn :: [(Float, Bool)] -> Interval
  --let randomNum = random :: StdGen -> (Float, StdGen)
  --
  --let randFn = randomBox :: StdGen -> (BoundingBox, StdGen)
  --let learnFn = learn :: [(Point, Bool)] -> BoundingBox
  --let randomNum = randomPoint :: StdGen -> (Point, StdGen)
  --
  let randFn = randomLiteralVector 10 :: StdGen -> (LiteralVector, StdGen)
  let learnFn = learn :: [(BoolVector, Bool)] -> LiteralVector
  let randomNum = randomBoolVector 10 :: StdGen -> (BoolVector, StdGen)

  putStrLn $ show $ pacEvaluate learnFn randFn randomNum 10 g
