import System.Random

class (Eq x) => LabelSpace x where
  notLabel :: x -> x

class (Ord x) => InputSpace x

instance InputSpace Float

class Concept c where
  apply :: (InputSpace x, LabelSpace y) => c x y -> x -> y
  learn :: (InputSpace x, LabelSpace y) => y -> [(x, y)] -> c x y
  randomConcept :: (InputSpace x, LabelSpace y) => y -> (StdGen -> (x, StdGen)) -> StdGen -> (c x y, StdGen)

labelData :: (Concept c, InputSpace x, LabelSpace y) => c x y -> [x] -> [(x, y)]
labelData concept dataList = map (\x -> (x, apply concept x)) dataList

isCorrect :: (Concept c, InputSpace x, LabelSpace y) => c x y -> (x, y) -> Bool
isCorrect c (x, y) = (apply c x) == y

isIncorrect :: (Concept c, InputSpace x, LabelSpace y) => c x y -> (x, y) -> Bool
isIncorrect = ((not .) . isCorrect)

errorOf :: (Concept c, InputSpace x, LabelSpace y) => c x y -> [(x, y)] -> Float
errorOf c dataList = 
  let incorrectList = filter (\x -> isIncorrect c x) dataList
      total = (fromIntegral (length dataList))::Float
      totalIncorrect = (fromIntegral (length incorrectList))::Float
  in totalIncorrect / total

evaluateConcepts :: (Concept c, Concept h, InputSpace x, LabelSpace y) => c x y -> h x y -> (StdGen -> (x, StdGen)) -> StdGen -> Float
evaluateConcepts concept hypothesis dataGen g = let
  (testPoints, g1) = generateDataset 10000 g dataGen
  labeledTestPoints = labelData concept testPoints
  in (errorOf hypothesis labeledTestPoints)

data BinaryLabel = Is | IsNot 
        deriving (Eq)

instance LabelSpace BinaryLabel where
  notLabel Is = IsNot
  notLabel IsNot = Is

data Interval x y = Interval { lower::x, upper::x, label::y }

isin :: (InputSpace x, LabelSpace y) => Interval x y -> x -> Bool
isin (Interval lower upper label) val = if (val >= lower) && (val <= upper)
                                   then True
                                else False

instance Concept Interval where
  apply interval@(Interval upper lower label) val = if isin interval val
                                                  then label
                                                  else notLabel label

  learn label dataList = let positive_examples = filter (\x -> snd x == label) dataList
                             positive_points = map (\x -> fst x) positive_examples
                         in if (length positive_points) > 0
                            then Interval (minimum positive_points) (maximum positive_points) label
                            else Interval (fst (dataList !! 0)) (fst (dataList !! 0)) label

  randomConcept label genValue g =
    let (valOne, g1) = genValue g
        (valTwo, g2) = genValue g1
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

pacEvaluate :: (Concept c, InputSpace x, LabelSpace y) => Int -> ([(x, y)] -> c x y) -> StdGen -> (StdGen -> (c x y, StdGen)) -> (StdGen -> (x, StdGen)) -> Float
pacEvaluate numTrain learningFun g generateConcept valGenerator = let
  (hiddenConcept, g1) = generateConcept g
  (trainPoints, g2) = generateDataset numTrain g1 valGenerator 
  labeledTrainPoints = labelData hiddenConcept trainPoints
  learnedConcept = learningFun labeledTrainPoints
  in (evaluateConcepts hiddenConcept learnedConcept valGenerator g2)

main  = do
  g <- getStdGen
  let distribution = random :: (StdGen -> (Float, StdGen))
  let learnFn = learn Is :: [(Float, BinaryLabel)] -> Interval Float BinaryLabel
  let randomGen = (randomConcept Is distribution) :: (StdGen -> (Interval Float BinaryLabel, StdGen))
  putStrLn $ show $ pacEvaluate 10 learnFn g randomGen distribution 
