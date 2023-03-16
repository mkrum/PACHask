
set terminal png
set output "static/pac/bool.png"

set ylabel "delta"
set xlabel "m"

plot "data/bool2.dat" w lp title "2",\
     "data/bool4.dat" w lp title "4",\
     "data/bool8.dat" w lp title "8",\
     "data/bool16.dat" w lp title "16"\

