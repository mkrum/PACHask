
all: box interval boolvector

pac: PAC.hs
	ghc PAC.hs

interval: interval.hs pac
	ghc interval.hs

box: box.hs pac
	ghc box.hs

boolvector: boolvector.hs pac
	ghc boolvector.hs

intervalData: interval
	./interval > data/interval.dat

boxData: interval
	./box > data/box.dat

boolvectorData: boolvector
	./boolvector > data/boolvector.dat
