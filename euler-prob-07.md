# Project Euler Problem 7
<!-- Vim: set filetype=markdown spell : -->

(2014-10-05 wachr) This problem requires computing the nth prime number. This
is a rather complex affair that necessitates some degree of design or planning.

I am planning to implement the algorithm described on the [algorithm][1] page
of the Prime Database website.

(2014-10-06 wachr) Prior to that (and perhaps instead of), I plan to implement
in C++ the Sieve of Eratosthenes as described in ["Prime sieves using binary
quadratic forms"][3] by Atkin and Bernstein, which was linked from the
Wikipedia article on [generating primes][2]. I may also implement some of the
other algorithms from the paper, but I am thinking that for my purposes, the
simplest sieve should be sufficient.

## Summary of Algorithm

### Sieve of Eratosthenes

(2014-10-07 wachr) For the simple sieve, I just want to create a function that
will produce an array of all the prime numbers between a high and a low value.

(2014-10-08 wachr) Note, that this is not exactly the algorithm presented in
the Atkin paper, which includes a number of straightforward optimizations. Some
of these optimizations should probably be included in my implementation to
avoid extraneous computation and to make the implementation a little more
robust and reusable, but I am trying to forego any premature optimization at
this stage; thus the divergence.

### Nth Prime

(2014-10-08 wachr) Now that I have a working (tested) sieve of Eratosthenes, I
can implement an Nth prime function that simply calls the sieve multiple times
until the sum of the number of primes found is equal to N.

Admittedly, this is somewhat naive in that the sieve I currently have is
entirely unoptimized and is not quite capable of being run for any range in
that way. *The first step will have to be to write more tests to support
running `simpleSieve` over arbitrary ranges of integers.* As I refine the
function to pass these tests, I am confident that I will come across necessary
optimizations to streamline the function without the need to rigorously analyze
my algorithm by hand.

> #### I'm done
>
> (2014-10-08 wachr) So actually, just by implementing a `main` method that
> parses out command line arguments for the floor and ceiling values of the
> range to sieve and the maximum number of primes to display, I was able to
> find the answer to the particular Nth Prime problem presented me by some
> simple trial and error.
>
> I may revisit this code in the future to implement optimizations or perhaps
> some of the more elaborate algorithms, but for now, I have what I need out of
> it.

<!-- links -->
[1]: https://primes.utm.edu/nthprime/algorithm.php "The Prime Database"
[2]: http://en.wikipedia.org/wiki/Generating_primes "Wikipedia: Generating Primes"
[3]: http://cr.yp.to/papers/primesieves-19990826.pdf "Prime Sieves paper (PDF)"
