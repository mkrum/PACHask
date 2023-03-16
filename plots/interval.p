
set terminal png
set output "static/pac/interval.png"

set ylabel "delta"
set xlabel "m"

f(x) = 2 * exp(-x * 0.01 / 2)
plot "data/interval.dat" w lp title "Experimental", f(x) title "Theoretical Bound"
