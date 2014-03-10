set term wxt size 800, 600
set grid
set xtics 5
set xlabel "threads"
set ylabel "transactions per second"
mod(x,n) = x - floor(x/n) * n
label_offset(x) = 17 * (2 * mod(x+1, 2) - 1)

plot [0:50][0:1250] \
  "gwlock.txt" using 1:3 with lines lw 2 lc 1 title "global write lock", \
  "stmx.txt" using 1:3 with lines lw 2 lc 2 title "stmx transactions" , \
  "single.txt" using 1:3 with lines lw 2 lc 3 title "single threaded"
