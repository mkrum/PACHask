{-# LANGUAGE MultiParamTypeClasses #-}

import System.Random

class (Eq x) => LabelSpace x where
  notLabel :: x -> x

class InputSpace x

class (InputSpace x, LabelSpace y) => Concept c x y where
  apply :: c x y -> x -> y
  learn :: y -> [(x, y)] -> c x y
  randomConcept :: y -> StdGen -> (c x y, StdGen)

labelData :: (Concept c x y) => c x y -> [x] -> [(x, y)]
labelData concept dataList = map (\x -> (x, apply concept x)) dataList

isCorrect :: (Concept c x y) => c x y -> (x, y) -> Bool
isCorrect c (x, y) = (apply c x) == y

isIncorrect :: (Concept c x y) => c x y -> (x, y) -> Bool
isIncorrect = ((not .) . isCorrect)

errorOf :: (Concept c x y) => c x y -> [(x, y)] -> Float
errorOf c dataList = 
  let incorrectList = filter (\x -> isIncorrect c x) dataList
      total = (fromIntegral (length dataList))::Float
      totalIncorrect = (fromIntegral (length incorrectList))::Float
  in totalIncorrect / total

evaluateConcepts :: (Concept c x y, Concept h x y) => c x y -> h x y -> (StdGen -> (x, StdGen)) -> StdGen -> (Float, StdGen)
evaluateConcepts concept hypothesis dataGen g = let
  (testPoints, g1) = generateDataset 10000 g dataGen
  labeledTestPoints = labelData concept testPoints
  in ((errorOf hypothesis labeledTestPoints), g1)

instance InputSpace Float

data BinaryLabel = Is | IsNot 
        deriving (Eq)

instance LabelSpace BinaryLabel where
  notLabel Is = IsNot
  notLabel IsNot = Is

data Interval x y = Interval { lower::x, upper::x, label::y}

isin :: (LabelSpace y) => Interval Float y -> Float -> Bool
isin (Interval lower upper label) val = (val >= lower) && (val <= upper)

instance Concept Interval Float BinaryLabel where
  apply interval@(Interval upper lower label) val = if isin interval val
                                                  then label
                                                  else notLabel label

  learn label dataList = let positive_examples = filter (\x -> snd x == label) dataList
                             positive_points = map (\x -> fst x) positive_examples
                         in if (length positive_points) > 0
                            then Interval (minimum positive_points) (maximum positive_points) label
                            else Interval (fst (dataList !! 0)) (fst (dataList !! 0)) label

  randomConcept label g =
    let (valOne, g1) = random g
        (valTwo, g2) = random g1
   in if valOne < valTwo
      then (Interval valOne valTwo label, g2)
      else (Interval valTwo valOne label, g2)

chainGenerate :: StdGen -> (StdGen -> (x, StdGen)) -> [(x, StdGen)]
chainGenerate g genFun = let (val, g1) = genFun g
                         in (val, g) : (chainGenerate g1 genFun)

generateDataset :: Int -> StdGen -> (StdGen -> (x, StdGen)) -> ([x], StdGen)
generateDataset num g valGenerator = let
 samples = take num (chainGenerate g valGenerator)
 points = map fst samples
 g1 = last $ map snd samples
 in (points, g1)

pacEvaluate :: (Concept c x y) => Int -> ([(x, y)] -> c x y) -> StdGen -> (StdGen -> (c x y, StdGen)) -> (StdGen -> (x, StdGen)) -> (Float, StdGen)
pacEvaluate numTrain learningFun g generateConcept valGenerator = let
  (hiddenConcept, g1) = generateConcept g
  (trainPoints, g2) = generateDataset numTrain g1 valGenerator 
  labeledTrainPoints = labelData hiddenConcept trainPoints
  learnedConcept = learningFun labeledTrainPoints
  in (evaluateConcepts hiddenConcept learnedConcept valGenerator g2)

data Point = Point {xValue :: Float, yValue :: Float}

instance InputSpace Point

randomPoint :: StdGen -> (Point, StdGen)
randomPoint g = let (xVal, g1) = random g
                    (yVal, g2) = random g1
                in ((Point xVal yVal), g2)

isin2d :: (LabelSpace y) => Interval Point y -> Point -> Bool
isin2d (Interval (Point x1 y1) (Point x2 y2) label) (Point inputX inputY) = and [(inputX >= x1), (inputX <= x2), (inputY >= y1), (inputY <= y2)]

instance Concept Interval Point BinaryLabel where
  apply interval@(Interval upper lower label) val = if isin2d interval val
                                                  then label
                                                  else notLabel label

  learn label dataList = let positive_examples = filter (\x -> snd x == label) dataList
                             positive_points = map (\x -> fst x) positive_examples
                         in if (length positive_points) > 0
                            then let xValues = map xValue positive_points
                                     yValues = map yValue positive_points
                                     lowerBounds = (Point (minimum xValues) (minimum yValues))
                                     upperBounds = (Point (maximum xValues) (maximum yValues))
                                  in Interval lowerBounds upperBounds label
                            else Interval (Point 1.0 1.0) (Point 1.0 1.0) label

  randomConcept label g =
    let ((Point x1 y1), g1) = randomPoint g
        ((Point x2 y2), g2) = randomPoint g1
        (xmin, xmax) = if x1 <= x2 then (x1, x2) else (x2, x1)
        (ymin, ymax) = if y1 <= y2 then (y1, y2) else (y2, y1)
   in ((Interval (Point xmin ymin) (Point xmax ymax) label), g2)

main  = do
  g <- getStdGen
  let learnFn = learn Is :: [(Point, BinaryLabel)] -> Interval Point BinaryLabel
  let randomGen = randomConcept Is 
  putStrLn $ show $ fst $ pacEvaluate 100 learnFn g randomGen randomPoint
