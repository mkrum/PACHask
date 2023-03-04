
import PAC
import Control.Monad.Random

main = do
    let intervalPAC = (getRandom, randomInterval, learnInterval, 0.01) :: PACTuple Float Bool
    outputData intervalPAC 300 25 600
