import System.Random

class (Eq x) => LabelSpace x where
  notLabel :: x -> x

class Concept c where
  apply :: (Ord x, LabelSpace y) => c x y -> x -> y
  learn :: (Ord x, LabelSpace y) => [(x, y)] -> y -> c x y
  randomConcept :: (Ord x, LabelSpace y) => StdGen -> (StdGen -> (x, StdGen)) -> y -> (c x y, StdGen)

labelData :: (Ord x, LabelSpace y, Concept c) => c x y -> [x] -> [(x, y)]
labelData concept dataList = map (\x -> (x, apply concept x)) dataList

isCorrect :: (Ord x, Concept c, LabelSpace y) => c x y -> (x, y) -> Bool
isCorrect c (x, y) = (apply c x) == y

isIncorrect :: (Ord x, Concept c, LabelSpace y) => c x y -> (x, y) -> Bool
isIncorrect = ((not .) . isCorrect)

errorOf :: (Ord x, Concept c, LabelSpace y) => c x y -> [(x, y)] -> Float
errorOf c dataList = 
  let incorrectList = filter (\x -> isIncorrect c x) dataList
      total = (fromIntegral (length dataList))::Float
      totalIncorrect = (fromIntegral (length incorrectList))::Float
  in totalIncorrect / total

data BinaryLabel = Is | IsNot 
        deriving (Eq)

instance LabelSpace BinaryLabel where
  notLabel Is = IsNot
  notLabel IsNot = Is

data Interval x y = Interval { lower::x, upper::x, label::y }

isin :: (Ord x, LabelSpace y) => Interval x y -> x -> Bool
isin (Interval lower upper label) val = if (val >= lower) && (val <= upper)
                                   then True
                                else False

instance Concept Interval where
  apply interval@(Interval upper lower label) val = if isin interval val
                                                  then label
                                                  else notLabel label

  learn dataList label = let positive_examples = filter (\x -> snd x == label) dataList
                             positive_points = map (\x -> fst x) positive_examples
                         in if (length positive_points) > 0
                            then Interval (minimum positive_points) (maximum positive_points) label
                            else Interval (fst (dataList !! 0)) (fst (dataList !! 0)) label

  randomConcept g genValue label =
    let (valOne, g1) = genValue g
        (valTwo, g2) = genValue g1
   in if valOne < valTwo
      then (Interval valOne valTwo label, g2)
      else (Interval valTwo valOne label, g2)

main  = do putStrLn "hello"
--  labelData :: c -> [x] -> [(x, y)]
--  labelData concept dataList = map (\x -> (x, apply concept x)) dataList
--
--  isCorrect :: (Eq y) => c -> (x, y) -> Bool
--  isCorrect c (x, y) = (apply c x) == y
--
--  isIncorrect :: (Eq y) => c -> (x, y) -> Bool
--  isIncorrect = ((not .) . isCorrect)
--
--  errorOf :: (Eq y) => c -> [(x, y)] -> Float
--  errorOf c dataList = 
--    let incorrect_list = filter (\x -> isIncorrect c x) dataList
--        total = (fromIntegral (length dataList))::Float
--        total_incorrect = (fromIntegral (length incorrect_list))::Float
--    in total_incorrect / total
--
--instance Concept Interval Float Label where
--
--  apply interval x = if isin interval x then Is else IsNot
--
--  learn dataList = 
--    let positive_examples = filter (\x -> snd x == Is) dataList
--        positive_points = map (\x -> fst x) positive_examples
--     in if (length positive_points) > 0
--        then Interval (minimum positive_points) (maximum positive_points)
--        else Interval (fst (dataList !! 0)) (fst (dataList !! 0))
--
--instance RandomConcept Interval where
--  randomConcept g = 
--    let (valOne, g1) = (random g) :: (Float, StdGen)
--        (valTwo, g2) = (random g1) :: (Float, StdGen)
--    in if valOne < valTwo
--       then (Interval valOne valTwo, g2)
--       else (Interval valTwo valOne, g2)
--
--
--pacEvaluate :: (RandomConcept c, Concept c x y) => (g -> [x]) -> Float
--pacEvaluate dataGen = let (hiddenConcept, g1) = randomConcept g :: (c, StdGen)
--                        dataGenerator = dataGen g1 :: [x]
--                        trainPoints = take numTrain dataGenerator
--                        labledTrainPoints = (labelData hiddenConcept trainPoints) :: [(x, y)]
--                        learnedConcept = (learn labledTrainPoints) :: c
--                        evalPoints = take 10000 dataGenerator
--                        labledEvalPoints = (labelData hiddenConcept evalPoints) :: [(x, y)]
--                    in (errorOf interval labledEvalPoints)
--
--main = do putStrLn "hello"
--learnInterval :: [(Float, Label)] -> (Interval Float)
--learnInterval dataList = 
--    let positive_examples = filter (\x -> snd x == Is) dataList
--        positive_points = map (\x -> fst x) positive_examples
--     in if (length positive_points) > 0
--        then Interval (minimum positive_points) (maximum positive_points)
--        else Interval (fst (dataList !! 0)) (fst (dataList !! 0))
--
--        
--pacEvaluate :: Int -> StdGen -> [Float]
--pacEvaluate numTrain g =
--        let (hidden_concept, g1) = randomInterval g :: (Interval Float, StdGen)
--            dataGenerator = randoms g1 :: [Float]
--            trainPoints = take numTrain dataGenerator
--            labledTrainPoints = (labelData hidden_concept trainPoints) :: [(Float, Label)]
--            interval = (learn labledTrainPoints) :: Interval Float
--            evalPoints = take 10000 dataGenerator
--            labledEvalPoints = (labelData hidden_concept evalPoints) :: [(Float, Label)]
--        in (errorOf interval labledEvalPoints):pacEvaluate numTrain g1
--
--getResults :: StdGen -> Int -> Int -> Float
--getResults g total numTrain = 
--        let res = take total $ pacEvaluate numTrain g
--            successes = (length $ filter (< 0.01) res) 
--            p_success = (fromIntegral successes :: Float) / (fromIntegral total :: Float)
--        in p_success
--
--instance Concept ((Interval Float), Interval Float) (Float, Float) Label where 
--    --eval :: [(Interval Float, Interval Float)] -> (Float, Float) -> Label
--    eval (intOne, intTwo) (x, y) = if and [(isin intOne x), (isin intTwo y)] then Is else IsNot
--    
--    learn = learnInterval2d
--
--learnInterval2d dataList = 
--    let positive_examples = filter (\x -> snd x == Is) dataList
--        examples = map fst positive_examples 
--        xPoints = map (\x -> (fst x, Is)) examples :: [(Float, Label)]
--        yPoints = map (\x -> (snd x, Is)) examples :: [(Float, Label)]
--        x_interval = learnInterval xPoints :: Interval Float
--        y_interval = learnInterval yPoints :: Interval Float
--    in (x_interval, y_interval)
--
--randomInterval2d:: StdGen -> ((Interval Float, Interval Float), StdGen)
--
--randomInterval2d g = let (xInterval, g1) = randomInterval g
--                         (yInterval, g2) = randomInterval g1
--                     in ((xInterval, yInterval), g2)
--
--listToTuple:: [Float] -> [(Float, Float)]
--listToTuple (k:v:t) = (k, v) : listToTuple t
--
--pacEvaluate2d :: Int -> StdGen -> [Float]
--pacEvaluate2d numTrain g =
--        let (hidden_concept, g1) = randomInterval2d g
--
--            dataGenerator = listToTuple (randoms g1) :: [(Float, Float)]
--            trainPoints = take numTrain dataGenerator  :: [(Float, Float)]
--
--            labledTrainPoints = (labelData hidden_concept trainPoints) :: [((Float, Float), Label)]
--            learnedConcept = (learn labledTrainPoints) :: (Interval Float, Interval Float)
--
--            evalPoints = take 10000 dataGenerator :: [(Float, Float)]
--            labledEvalPoints = (labelData learnedConcept evalPoints) :: [((Float, Float), Label)]
--
--        in (errorOf learnedConcept labledEvalPoints):pacEvaluate numTrain g1
--
--getResults2d :: StdGen -> Int -> Int -> Float
--getResults2d g total numTrain = 
--        let res = take total $ pacEvaluate2d numTrain g
--            successes = (length $ filter (< 0.01) res) 
--            p_success = (fromIntegral successes :: Float) / (fromIntegral total :: Float)
--        in p_success
--
--main :: IO ()
--main = do
--        g <- getStdGen
--        let total = 1000 :: Int
--        let numTrain = [5,10..500] :: [Int]
--        let p_success = map (getResults2d g total) numTrain
--        let outputStr = map (\x -> ((show . fst) x) ++ " " ++ ((show . snd) x) ++ "\n") (zip numTrain p_success)
--        putStr $ foldl (++) " " outputStr
