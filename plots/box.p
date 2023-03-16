
set terminal png
set output "static/pac/box.png"

set ylabel "delta"
set xlabel "m"

f(x) = 4 * exp(-x * 0.01 / 4)
plot "data/box.dat" w lp title "Experimental", f(x) title "Theoretical Bound"
