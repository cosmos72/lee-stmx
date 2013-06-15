# set term wxt size 800, 600
set grid
set xtics 5
set ytics 0.2
set xlabel "threads"
set ylabel "performance ratio"
set style fill solid border -1
set boxwidth 0.67 

mod(x,n) = x - floor(x/n) * n
label_offset(x) = 0.015 * (2 * mod(x+1, 2) - 1)

plot [0:51][0:] \
  "stmx-div-single.txt" using 1:($3/$7) title "stmx transactions per second / single thread" with boxes lc 3, \
  \
  "stmx-div-gwlock.txt" using 1:($3/$7) title "stmx transactions per second / global write lock" with boxes lc 1