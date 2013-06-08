Lee-STMX
========

Lee-STMX is a benchmark for [STMX](https://github.com/cosmos72/stmx), a high-performance software
transactional memory library for Common Lisp.

STMX is available from [GitHub](https://github.com/cosmos72/stmx),
more information about it can be found in its
[README](https://github.com/cosmos72/stmx/blob/master/README.md).

A general introduction on software transactional memory is available
in many places, including at least [Wikipedia](http://en.wikipedia.org/wiki/Software_transactional_memory)
and several research papers. One of the most introductory, but still
quite technical is [Composable Memory Transaction](http://research.microsoft.com/%7Esimonpj/papers/stm/stm.pdf)
from Microsoft research.

Lee-STMX is a porting of [Lee-TM](http://apt.cs.man.ac.uk/projects/TM/LeeBenchmark/)
to STMX. It is a non-trivial benchmark suite for transactional memory
developed in 2007 by the University of Manchester (UK), then ported to
STMX by the STMX author.

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

Software: Debian GNU/Linux 7.0 (x86_64), SBCL 1.1.7 (x86_64), STMX 1.3.2

Benchmark results for MEMBOARD circuit:
![nil](./results/memboard.png)

<table>
 <tr><td rowspan="23">mainboard</td>

     <td>single threaded</td>  <td> 1</td><td>11.842</td><td>127.2</td><td> 0.0</td></tr>
 <tr><td rowspan="11">STMX</td><td> 1</td><td>18.579</td><td> 81.1</td><td> 0.0</td></tr>
 <tr>                          <td> 2</td><td>10.262</td><td>146.8</td><td> 4.5</td></tr>
 <tr>                          <td> 3</td><td> 7.413</td><td>203.2</td><td>10.8</td></tr>
 <tr>                          <td> 4</td><td> 6.120</td><td>246.1</td><td>19.6</td></tr>
 <tr>                          <td> 5</td><td> 6.425</td><td>234.4</td><td>20.2</td></tr>
 <tr>                          <td> 6</td><td> 6.448</td><td>233.6</td><td>25.0</td></tr>
 <tr>                          <td> 8</td><td> 7.070</td><td>213.0</td><td>26.4</td></tr>
 <tr>                          <td>10</td><td> 7.742</td><td>194.5</td><td>28.4</td></tr>
 <tr>                          <td>20</td><td> 8.498</td><td>177.2</td><td>37.8</td></tr>
 <tr>                          <td>30</td><td> 9.452</td><td>159.3</td><td>46.4</td></tr>
 <tr>                          <td>50</td><td>10.489</td><td>143.6</td><td>54.5</td></tr>
 <tr><td rowspan="11">global write lock + undo buffer</td>
                               <td> 1</td><td>11.821</td><td>127.4</td><td> 0.0</td></tr>
 <tr>                          <td> 2</td><td> 6.901</td><td>218.2</td><td> 6.5</td></tr>
 <tr>                          <td> 3</td><td> 5.061</td><td>297.6</td><td>16.6</td></tr>
 <tr>                          <td> 4</td><td> 5.421</td><td>277.8</td><td>22.7</td></tr>
 <tr>                          <td> 5</td><td> 4.583</td><td>328.6</td><td>26.8</td></tr>
 <tr>                          <td> 6</td><td> 4.460</td><td>337.7</td><td>30.0</td></tr>
 <tr>                          <td> 8</td><td> 4.917</td><td>306.3</td><td>34.8</td></tr>
 <tr>                          <td>10</td><td> 5.148</td><td>292.5</td><td>39.4</td></tr>
 <tr>                          <td>20</td><td> 5.513</td><td>273.2</td><td>56.4</td></tr>
 <tr>                          <td>30</td><td> 6.187</td><td>243.4</td><td>60.4</td></tr>
 <tr>                          <td>50</td><td> 6.737</td><td>223.5</td><td>78.2</td></tr>
</table>



Legal
-----

Lee-STMX is released under the terms of the
[BSD](http://opensource.org/licenses/BSD-3-Clause) license.

STMX is released under the terms of the [Lisp Lesser General Public
License](http://opensource.franz.com/preamble.html), known as the LLGPL.
