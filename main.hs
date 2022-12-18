{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


import System.Environment
import System.Random

class (Eq y) => Concept c x y where 

    eval :: c -> x -> y

    learn :: [(x, y)] -> c

    labelData :: c -> [x] -> [(x, y)]
    labelData concept dataList = map (\x -> (x, eval concept x)) dataList

    isCorrect :: c -> (x, y) -> Bool
    isCorrect c (x, y) = (eval c x) == y

    isIncorrect :: c -> (x, y) -> Bool
    isIncorrect = ((not .) . isCorrect)

    errorOf :: c -> [(x, y)] -> Float
    errorOf c dataList = 
            let incorrect_list = filter (\x -> isIncorrect c x) dataList
                total = (fromIntegral (length dataList))::Float
                total_incorrect = (fromIntegral (length incorrect_list))::Float
            in total_incorrect / total

data Labels = Is | IsNot 
        deriving (Eq)

data Interval a = Interval a a

isin::(Ord a) => Interval a -> a -> Bool
isin (Interval lower upper) x = (x >= lower) && (x <= upper)

randomInterval:: StdGen -> (Interval Float, StdGen)
randomInterval g = 
    let (valOne, g1) = (random g) :: (Float, StdGen)
        (valTwo, g2) = (random g1) :: (Float, StdGen)
    in if valOne < valTwo
          then (Interval valOne valTwo, g2)
          else (Interval valTwo valOne, g2)

instance Concept (Interval Float) Float Labels where

    eval interval x = if isin interval x then Is else IsNot

    learn dataList = 
            let positive_examples = filter (\x -> snd x == Is) dataList
                positive_points = map (\x -> fst x) positive_examples
             in if (length positive_points) > 0
                then Interval (minimum positive_points) (maximum positive_points)
                else Interval (fst (dataList !! 0)) (fst (dataList !! 0))

pacEvaluate :: Int -> StdGen -> [Float]
pacEvaluate numTrain g =
        let (hidden_concept, g1) = randomInterval g :: (Interval Float, StdGen)
            dataGenerator = randoms g1 :: [Float]
            trainPoints = take numTrain dataGenerator
            labledTrainPoints = (labelData hidden_concept trainPoints) :: [(Float, Labels)]
            interval = (learn labledTrainPoints) :: Interval Float
            evalPoints = take 10000 dataGenerator
            labledEvalPoints = (labelData hidden_concept evalPoints) :: [(Float, Labels)]
        in (errorOf interval labledEvalPoints):pacEvaluate numTrain g1

getResults :: StdGen -> Int -> Int -> Float
getResults g total numTrain = 
        let res = take total $ pacEvaluate numTrain g
            successes = (length $ filter (< 0.10) res) 
            p_success = (fromIntegral successes :: Float) / (fromIntegral total :: Float)
        in p_success

main :: IO ()
main = do
        args <- getArgs
        g <- getStdGen
        let total = 100 :: Int
        let numTrain = [5,10..100] :: [Int]
        let p_success = map (getResults g total) numTrain
        let outputStr = map (\x -> ((show . fst) x) ++ " " ++ ((show . snd) x) ++ "\n") (zip numTrain p_success)
        putStr $ foldl (++) " " outputStr
