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

     <td>single threaded</td>  <td> 1</td><td> 8.945</td><td>346.7</td><td> 0.0</td></tr>
 <tr><td rowspan="11">STMX</td><td> 1</td><td>12.792</td><td>242.4</td><td> 0.0</td></tr>
 <tr>                          <td> 2</td><td> 8.061</td><td>384.7</td><td> 8.1</td></tr>
 <tr>                          <td> 3</td><td> 6.517</td><td>475.8</td><td>17.2</td></tr>
 <tr>                          <td> 4</td><td> 5.653</td><td>548.6</td><td>26.9</td></tr>
 <tr>                          <td> 5</td><td> 5.688</td><td>545.2</td><td>29.0</td></tr>
 <tr>                          <td> 6</td><td> 5.747</td><td>539.8</td><td>28.7</td></tr>
 <tr>                          <td> 8</td><td> 6.188</td><td>501.1</td><td>40.1</td></tr>
 <tr>                          <td>10</td><td> 6.112</td><td>507.4</td><td>34.8</td></tr>
 <tr>                          <td>20</td><td> 7.335</td><td>422.8</td><td>53.6</td></tr>
 <tr>                          <td>30</td><td> 7.927</td><td>391.2</td><td>66.0</td></tr>
 <tr>                          <td>50</td><td> 8.881</td><td>349.2</td><td>74.5</td></tr>

 <tr><td rowspan="12">mainboard</td>

     <td>single threaded</td>  <td> 1</td><td>12.216</td><td>123.3</td><td> 0.0</td></tr>
 <tr><td rowspan="11">STMX</td><td> 1</td><td>19.421</td><td> 77.5</td><td> 0.0</td></tr>
 <tr>                          <td> 2</td><td>12.035</td><td>125.1</td><td> 4.3</td></tr>
 <tr>                          <td> 3</td><td> 9.052</td><td>166.4</td><td> 9.3</td></tr>
 <tr>                          <td> 4</td><td> 7.478</td><td>201.4</td><td>16.3</td></tr>
 <tr>                          <td> 5</td><td> 7.244</td><td>207.9</td><td>16.2</td></tr>
 <tr>                          <td> 6</td><td> 7.458</td><td>201.9</td><td>18.8</td></tr>
 <tr>                          <td> 8</td><td> 7.876</td><td>191.2</td><td>24.8</td></tr>
 <tr>                          <td>10</td><td> 8.097</td><td>186.0</td><td>27.3</td></tr>
 <tr>                          <td>20</td><td> 8.983</td><td>167.7</td><td>35.6</td></tr>
 <tr>                          <td>30</td><td>10.549</td><td>142.8</td><td>44.3</td></tr>
 <tr>                          <td>50</td><td>10.540</td><td>142.9</td><td>50.4</td></tr>

</table>



Legal
-----

Lee-STMX is released under the terms of the
[BSD](http://opensource.org/licenses/BSD-3-Clause) license.

STMX is released under the terms of the [Lisp Lesser General Public
License](http://opensource.franz.com/preamble.html), known as the LLGPL.
