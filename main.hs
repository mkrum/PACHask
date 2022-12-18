{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import System.Random

class (Eq y) => Concept c x y where 

    eval :: c -> x -> y

    learn :: [(x, y)] -> c

    isCorrect :: c -> (x, y) -> Bool
    isCorrect c (x, y) = (eval c x) == y

    errorOf :: c -> [(x, y)] -> Float
    errorOf c dataList = 
            let correct_list = filter (\x -> isCorrect c x) dataList
                total = (fromIntegral (length dataList))::Float
                total_correct = (fromIntegral (length correct_list))::Float
            in total_correct / total

    labelData :: c -> [x] -> [(x, y)]
    labelData concept dataList = map (\x -> (x, eval concept x)) dataList


data Labels = Is | IsNot 
        deriving (Eq)

data Interval a = Interval a a

isin::(Ord a) => Interval a -> a -> Bool
isin (Interval lower upper) x = (x >= lower) && (x <= upper)

instance (Num a, Ord a) => Concept (Interval a) a Labels where

    eval interval x = if isin interval x then Is else IsNot

    learn dataList = 
            let positive_examples = filter (\x -> snd x == Is) dataList
                positive_points = map (\x -> fst x) positive_examples
             in if (length positive_points) > 0
                then Interval (minimum positive_points) (maximum positive_points)
                else Interval (fst (dataList !! 0)) (fst (dataList !! 0))


main :: IO ()
main = do
        g <- getStdGen

        let hidden_concept = (Interval 2.4 7.4) :: Interval Float

        let dataGenerator = randomRs (0, 10) g :: [Float]

        let trainPoints = take 5 dataGenerator
        let labledTrainPoints = (labelData hidden_concept trainPoints) :: [(Float, Labels)]

        let interval = (learn labledTrainPoints) :: Interval Float

        let evalPoints = take 100000 dataGenerator
        let labledEvalPoints = (labelData hidden_concept evalPoints) :: [(Float, Labels)]
        let res = (errorOf interval labledEvalPoints) :: Float

        putStrLn . show $ res
