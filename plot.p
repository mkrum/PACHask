
set term tikz 
set output 'example.tex'

set xlabel "Samples"
set ylabel "$P(\\epsilon < \\delta)$"

plot "data/interval.dat" using 2:1 with lines title "Interval"
