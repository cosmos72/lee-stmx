set term wxt size 800, 600
set grid
set xtics 5
set xlabel "threads"
set ylabel "transactions per second"
mod(x,n) = x - floor(x/n) * n
label_offset(x) = 17 * (2 * mod(x+1, 2) - 1)

plot [0:51][0:] \
  "gwlock.txt" using 1:3 title "global write lock [SUCCESS]" with linespoints pt 13 lc 1, \
  "" using 1:($3-1.5*label_offset($1)):($3) notitle with labels, \
  \
  "gwlock.txt" using 1:4 title "global write lock [RETRIED]" with linespoints pt 2 lc 1, \
  \
  "stmx.txt" using 1:3 title "stmx transactions [SUCCESS]" with linespoints pt 13 lc 2, \
  "" using 1:($3-0.8*label_offset($1)):($3) notitle with labels, \
  \
  "stmx.txt" using 1:4 title "stmx transactions [RETRIED]" with linespoints pt 2 lc 2, \
  \
  "single.txt" using 1:3 title "single threaded" with linespoints pt 13 lc 3, \
  "" using 1:($3+label_offset(0)):($6) notitle with labels
