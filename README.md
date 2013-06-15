Lee-STMX
========

Lee-STMX is a benchmark for [STMX](https://github.com/cosmos72/stmx), a high-performance software
transactional memory library for Common Lisp.

STMX is available from [GitHub](https://github.com/cosmos72/stmx),
more information about it can be found in its
[README](https://github.com/cosmos72/stmx/blob/master/README.md).

A general introduction on software transactional memory is available in many places,
including at least [Wikipedia](http://en.wikipedia.org/wiki/Software_transactional_memory)
and several research papers. One of the most introductory, but still quite technical is
[Composable Memory Transaction](http://research.microsoft.com/%7Esimonpj/papers/stm/stm.pdf)
from Microsoft research.

Lee-STMX is a porting of
[Lee-TM](http://apt.cs.man.ac.uk/projects/TM/LeeBenchmark/) to STMX. It is a
non-trivial benchmark suite for transactional memory developed in 2007 by the
University of Manchester (UK), then ported to STMX by the STMX author. It aims
at implementing a realistic workload and uses longer transactions than what
found in common "artificial" micro-benchmarks. 

Quite unfortunately, a straightforward porting of Lee-TM algorithm was found
to have a strongly unbalanced workload. More than 99.5% of the CPU time was 
spent in reading transactional memory from **outside** any transaction; only
less than 0.5% of the CPU time was actually spent inside transactions, 
including writing to transactional memory.   

For this reason, Lee-STMX benchmark has been modified to use a variant
of Hadlock routing algorithm which, beyond being much faster, spends a
smaller percentage of time in the read-only phase. The modified
algorithm is better, but still quite unbalanced: 92-94% of the CPU
time is still spent in reading transactional memory from outside any
transaction; less than 1% of the CPU time is spent in atomic
transactions that also write into transactional memory.      


Status
------

As of May 2013, Lee-STMX is being written by Massimiliano Ghilardi
and it is considered BETA quality by the author.

At the same date, STMX is considered to be stable.

Benchmark results
-----------------

What follows are some timings obtained on the authors's system, and by no means they
claim to be exact, absolute or reproducible: your mileage may vary.

Date: 08 June 2013

Hardware: Intel Core-i5 750 @4.0 GHz (quad-core), 16GB RAM

Software: Debian GNU/Linux 7.0 (x86_64), SBCL 1.1.8 (x86_64), STMX 1.3.2

Results for MAINBOARD benchmark:
![nil](./results/mainboard.png)

Results for MEMBOARD benchmark:
![nil](./results/memboard.png)


Analysis and comments
---------------------

- First, it is evident that in Lee-STMX benchmark, using STMX transactions is
always faster than single-threaded code as soon as STMX runs at least two
threads. Since STMX targets multi-threaded code, this is good news. 

With 4+ threads on a quad-core machine, STMX delivers up to 95-105% performance
improvement with respect to single-threaded code on this benchmark (the peak
performance ratio is 1257.0 / 614.8 = 2.044 = 204.4% on the MEMBOARD workload
with 5 threads). Good, but still far from the theorical maximum of 300%
improvement, i.e. quadruple speed.
Quite unexpectedly, the peak performance is not reached by using one thread
per core, but depends on the workload. In the two workloads shown above, the
peak is reached respectively at 10 and 5 threads.

- Second, on this benchmark STMX transactions are typically 10-20% slower than
highly optimized locking code: by assuming 100% as locking code performance,
STMX reaches 80-90% of it for almost any number of threads.
    - The best result is the MEMBOARD workload with 30 threads, where STMX
      reaches 93.6% of the locking code performance. 
    - The worst result is the MAINBOARD workload with 1 thread, where STMX
      reaches 71.8% of the locking code performance. 

This is encouraging. After **many** research papers discussing and analyzing
the overhead imposed by software transactional memory, and looking for
optimizations to reduce such overhead, the performance penalty of STMX
transactions is quite small - at least on this benchmark.   

The optimized locking code running on the same quad-code machine delivers up
to 125-155% performance improvement with respect to single-threaded code (the
peak performance ratio is 996.0 / 388.1 = 2.566 = 256.6% on the MEMBOARD
workload with 10 threads). Better than transactions, but still far from
quadruple speed.    

- Finally, as stated in the introduction, a deeper analysis of this benchmark
reveals that 92-94% of the time is spent in the function `EXPAND-FROM`, which
reads memory shared among the threads (in the STMX version of the benchmark,
it is transactional memory) but never writes into it. Only `BACKTRACK-FROM`,
which accounts for less than 1% of the execution time, actually writes into
memory shared among the threads (in the STMX version of the benchmark,
`BACKTRACK-FROM` is an atomic transaction).

This means that Lee-STMX benchmark, even after the modifications that
introduced the Hadlock routing algorithm, is almost read-only: writes to
shared/transactional memory are very few and grouped together, and it spends
large amounts of time without writing at all to shared/transactional memory.
For this reason the performance results risk having limited significance,
since most of the time is actualy spent outside transactions. On the other
hand, also the optimized locking code takes advantage heavily of this
unbalance, to the point of being implemented with unlocked reads and a global
write lock (plus some tricks borrowed by transactions to handle inconsistent
reads and failures).

Summary
-------

Lee-STMX is a benchmark for software transactional memory aiming at
implementing a realistic workload. STMX shows good performance on it: 95-105%
better than single-threaded code on a quad-core machine, yet behind highly
optimized locking code, which shows 125-155% better performance than
single-threaded code.

Quite unfortunately, Lee-STMX was found to have a strongly unbalanced
workload: it contains a high number of transactioanl memory reads (actually
executed outside transactions), but very few and grouped transactional memory
writes.

Since there are no obviously tunable parameters to change the ratio of reads
and writes, nor to change the ratio of time spent inside and outside
transactions, STMX author recommends further benchmarking to validate STMX
performance on workloads with more transactional writes and more time spent
inside transactions.

Legal
-----

Lee-STMX is released under the terms of the
[BSD](http://opensource.org/licenses/BSD-3-Clause) license.

STMX is released under the terms of the [Lisp Lesser General Public
License](http://opensource.franz.com/preamble.html), known as the LLGPL.
