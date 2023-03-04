
import PAC

main = do
    let boxPAC = (randomPoint, randomBox, learnBox, 0.01) :: PACTuple Point Bool
    outputData boxPAC 100 10 500
