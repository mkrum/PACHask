
import PAC
import System.Environment

main = do
    args <- getArgs
    let n = read $ head $ args
    let boolPAC = (randomBoolVector n, randomLiteralExpression n, learnLiteralExpression, 0.01) :: PACTuple BoolVector Bool
    outputData boolPAC 10 10 100
