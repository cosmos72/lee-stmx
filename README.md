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

     <td>single threaded</td>  <td> 1</td><td> 9.777</td><td>317.2</td><td> 0.0</td></tr>
 <tr><td rowspan="11">STMX</td><td> 1</td><td>14.798</td><td>209.6</td><td> 0.0</td></tr>
 <tr>                          <td> 2</td><td> 8.995</td><td>344.7</td><td> 7.4</td></tr>
 <tr>                          <td> 3</td><td> 7.010</td><td>442.4</td><td>15.5</td></tr>
 <tr>                          <td> 4</td><td> 6.128</td><td>506.0</td><td>24.0</td></tr>
 <tr>                          <td> 5</td><td> 6.172</td><td>502.4</td><td>25.3</td></tr>
 <tr>                          <td> 6</td><td> 6.089</td><td>509.3</td><td>27.4</td></tr>
 <tr>                          <td> 8</td><td> 6.598</td><td>470.0</td><td>35.3</td></tr>
 <tr>                          <td>10</td><td> 7.106</td><td>436.4</td><td>39.5</td></tr>
 <tr>                          <td>20</td><td> 7.757</td><td>399.8</td><td>46.0</td></tr>
 <tr>                          <td>30</td><td> 8.685</td><td>357.1</td><td>60.7</td></tr>
 <tr>                          <td>50</td><td> 9.363</td><td>331.2</td><td>69.8</td></tr>

 <tr><td rowspan="12">mainboard</td>

     <td>single threaded</td>  <td> 1</td><td>13.824</td><td>108.9</td><td> 0.0</td></tr>
 <tr><td rowspan="11">STMX</td><td> 1</td><td>22.390</td><td> 67.3</td><td> 0.0</td></tr>
 <tr>                          <td> 2</td><td>13.655</td><td>110.3</td><td> 3.1</td></tr>
 <tr>                          <td> 3</td><td> 9.716</td><td>155.0</td><td> 8.7</td></tr>
 <tr>                          <td> 4</td><td> 8.158</td><td>184.6</td><td>15.0</td></tr>
 <tr>                          <td> 5</td><td> 7.868</td><td>191.4</td><td>17.4</td></tr>
 <tr>                          <td> 6</td><td> 8.258</td><td>182.4</td><td>19.6</td></tr>
 <tr>                          <td> 8</td><td> 9.202</td><td>163.7</td><td>20.5</td></tr>
 <tr>                          <td>10</td><td> 8.846</td><td>170.2</td><td>25.0</td></tr>
 <tr>                          <td>20</td><td> 9.999</td><td>150.6</td><td>35.2</td></tr>
 <tr>                          <td>30</td><td>11.015</td><td>136.7</td><td>41.2</td></tr>
 <tr>                          <td>50</td><td>12.756</td><td>118.1</td><td>47.8</td></tr>

</table>



Legal
-----

Lee-STMX is released under the terms of the
[BSD](http://opensource.org/licenses/BSD-3-Clause) license.

STMX is released under the terms of the [Lisp Lesser General Public
License](http://opensource.franz.com/preamble.html), known as the LLGPL.
