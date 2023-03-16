
all: post.html

pac: PAC.hs
	ghc PAC.hs

interval: interval.hs PAC.hs
	ghc interval.hs

box: box.hs PAC.hs
	ghc box.hs

boolvector: boolvector.hs PAC.hs
	ghc boolvector.hs

data/interval.dat: interval
	./interval > data/interval.dat

data/box.dat: box
	./box > data/box.dat

data/bool2.dat: boolvector
	./boolvector 2 > data/bool2.dat

data/bool4.dat: boolvector
	./boolvector 4 > data/bool4.dat

data/bool8.dat: boolvector
	./boolvector 8 > data/bool8.dat

data/bool16.dat: boolvector
	./boolvector 16 > data/bool16.dat

static/pac/interval.png: data/interval.dat plots/interval.p
	gnuplot -s plots/interval.p

static/pac/box.png: plots/box.p data/box.dat
	gnuplot -s plots/box.p

static/pac/bool.png: data/bool2.dat data/bool4.dat data/bool8.dat data/bool16.dat plots/bool.p
	gnuplot -s plots/bool.p

post.html: post.org static/pac/bool.png static/pac/interval.png static/pac/box.png
	emacsclient -e "(progn (find-file \"post.org\") (org-html-export-to-html) (kill-buffer))"
