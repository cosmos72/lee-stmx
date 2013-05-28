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

Date: 27 May 2013

Hardware: Intel Core-i5 750 @4.0 GHz (quad-core), 16GB RAM

Software: Debian GNU/Linux 7.0 (x86_64), SBCL 1.1.6 (x86_64), STMX 1.3.2


<table>

 <tr><th rowspan="2"><b>benchmark</b></th>
     <th rowspan="2"><b>implementation</b></th>
     <th rowspan="2"><b>threads</b></th>
     <th rowspan="2"><b>elapsed time (seconds)</b></th>
     <th colspan="2"><b>connections per second</b></th></tr>

 <tr><th><b>successful</b></th>
     <th><b>retried</b></th></tr>

 <tr><td rowspan="12">memboard</td>

     <td>single threaded</td>  <td> 1</td><td> 9.770</td><td>317.4</td><td> 0.0</td></tr>
 <tr><td rowspan="11">STMX</td><td> 1</td><td>14.020</td><td>221.2</td><td> 0.0</td></tr>
 <tr>                          <td> 2</td><td> 8.061</td><td>384.7</td><td> 8.1</td></tr>
 <tr>                          <td> 3</td><td> 6.517</td><td>475.8</td><td>17.2</td></tr>
 <tr>                          <td> 4</td><td> 5.653</td><td>548.6</td><td>26.9</td></tr>
 <tr>                          <td> 5</td><td> 5.688</td><td>545.2</td><td>29.0</td></tr>
 <tr>                          <td> 6</td><td> 5.747</td><td>539.8</td><td>28.7</td></tr>
 <tr>                          <td> 8</td><td> 6.366</td><td>487.1</td><td>35.9</td></tr>
 <tr>                          <td>10</td><td> 7.106</td><td>436.4</td><td>39.5</td></tr>
 <tr>                          <td>20</td><td> 7.592</td><td>408.5</td><td>51.9</td></tr>
 <tr>                          <td>30</td><td> 8.367</td><td>370.6</td><td>61.2</td></tr>
 <tr>                          <td>50</td><td> 9.244</td><td>335.5</td><td>66.5</td></tr>

 <tr><td rowspan="12">mainboard</td>

     <td>single threaded</td>  <td> 1</td><td>13.580</td><td>110.9</td><td> 0.0</td></tr>
 <tr><td rowspan="11">STMX</td><td> 1</td><td>21.325</td><td> 70.6</td><td> 0.0</td></tr>
 <tr>                          <td> 2</td><td>11.648</td><td>129.3</td><td> 4.3</td></tr>
 <tr>                          <td> 3</td><td> 9.459</td><td>159.2</td><td> 9.4</td></tr>
 <tr>                          <td> 4</td><td> 7.738</td><td>194.6</td><td>14.6</td></tr>
 <tr>                          <td> 5</td><td> 7.340</td><td>205.2</td><td>17.7</td></tr>
 <tr>                          <td> 6</td><td> 7.496</td><td>200.9</td><td>18.7</td></tr>
 <tr>                          <td> 8</td><td> 8.202</td><td>183.6</td><td>24.8</td></tr>
 <tr>                          <td>10</td><td> 8.304</td><td>181.4</td><td>27.1</td></tr>
 <tr>                          <td>20</td><td> 9.040</td><td>166.6</td><td>30.5</td></tr>
 <tr>                          <td>30</td><td>10.736</td><td>140.3</td><td>43.2</td></tr>
 <tr>                          <td>50</td><td>11.682</td><td>128.9</td><td>45.3</td></tr>

</table>



Legal
-----

Lee-STMX is released under the terms of the
[BSD](http://opensource.org/licenses/BSD-3-Clause) license.

STMX is released under the terms of the [Lisp Lesser General Public
License](http://opensource.franz.com/preamble.html), known as the LLGPL.
