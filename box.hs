
import PAC

main = do
    let boxPAC = (randomPoint, randomBox, learnBox, 0.01) :: PACTuple Point Bool
    outputData boxPAC 300 25 1000
