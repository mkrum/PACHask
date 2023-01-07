{-# LANGUAGE MultiParamTypeClasses #-}

import System.Random

type Concept x y = x -> y

class BoolPACConcept a x where
    learn :: Bool -> [(x, Bool)] -> a
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
pacEvaluate numTrain learnFn generateConcept valGenerator g = let
  (hiddenConcept, g1) = generateConcept g 
  (trainPoints, g2) = generateDataset numTrain valGenerator g1 
  labeledTrainPoints = labelData (apply hiddenConcept) trainPoints 
  learnedConcept = learnFn labeledTrainPoints 
  in (evaluateConcepts (apply hiddenConcept) (apply learnedConcept) valGenerator g2)

data Interval = Interval { lower::Float, upper::Float, label::Bool}

isin :: Interval -> Float -> Bool
isin (Interval lower upper label) val = (val >= lower) && (val <= upper)

instance BoolPACConcept Interval Float where

  apply = isin 

  learn label dataList = 
        let positive_examples = filter (\x -> snd x == label) dataList
            positive_points = map (\x -> fst x) positive_examples
        in if (length positive_points) > 0
            then Interval (minimum positive_points) (maximum positive_points) label
            else Interval (fst (dataList !! 0)) (fst (dataList !! 0)) label

randomInterval :: Bool -> StdGen -> (Interval, StdGen)
randomInterval label g =
    let (valOne, g1) = random g
        (valTwo, g2) = random g1
   in if valOne < valTwo
      then (Interval valOne valTwo label, g2)
      else (Interval valTwo valOne label, g2)

main = do
  g <- getStdGen
  let randFn = randomInterval True :: StdGen -> (Interval, StdGen)
  let learnFn = learn True :: [(Float, Bool)] -> Interval
  let randomNum = random :: StdGen -> (Float, StdGen)
  putStrLn $ show $ pacEvaluate learnFn randFn randomNum 100 g
