#ifndef EULER_PROB_07_H
#define EULER_PROB_07_H

// An array of the first few primes for testing purposes.
const int FIRST_16_PRIMES[16] = {2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53};

// Test for primality of number by trial division.
bool trialDivision(int number);

// Simple as possible Sieve of Eratosthenes returns the number of primes found.
int simpleSieve(int *retArray, size_t arrSize, int lo, int hi);

#endif // EULER_PROB_07_H
