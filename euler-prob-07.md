# Project Euler Problem 7

This problem requires computing the nth prime number. This is a rather complex
affair that necessitates some degree of design or planning.

I am planning to implement the algorithm described on the [algorithm][1] page
of the Prime Database website.

Prior to that (and perhaps instead of), I plan to implement in C++ the Sieve of
Eratosthenes as described in ["Prime sieves using binary quadratic forms"][3],
which was linked from the Wikipedia article on [generating primes][2]. I may
also implement some of the other algorithms from the paper, but I am thinking
that for my purposes, the simplest sieve should be sufficient.

## Summary of Algorithm

### Sieve of Eratosthenes

For the simple sieve, I just want to create a function that will produce an
array of all the prime numbers between a high and a low value.

<!-- links -->
[1]: https://primes.utm.edu/nthprime/algorithm.php "The Prime Database"
[2]: http://en.wikipedia.org/wiki/Generating_primes "Wikipedia: Generating Primes"
[3]: http://cr.yp.to/papers/primesieves-19990826.pdf "Prime Sieves paper (PDF)"
