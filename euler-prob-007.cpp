/* euler-prob-007.cpp
 * author: Ray Wach
 * date: 2014-10-08
 * info: A couple simple prime sieve implementations.
 */

#include <math.h>
#include <stdio.h>
#include "euler-prob-07.h"

/* Check if the given integer has any integer factors greater than 1 and less
 * than itself.
 */
bool trialDivision(int number)
{
    if (number < 2) {
        return false;
    }
    int cap = floor(sqrt((double)number)) + 1;
    for (int i = 2; i < cap; i++) {
        if (number % i == 0) {
            return false;
        }
    }
    return true;
}

int simpleSieve(int *retArray, size_t arrSize, int lo, int hi)
{
    // Check for valid arguments.
    if (hi - lo > (int)arrSize) {
        fprintf(stderr, "simpleSieve passed array of insufficient size: ");
        fprintf(stderr, "requires array of length %d.\n", hi - lo);
        return 0;
    }
    // Don't check if 1, 0, or a negative number are prime.
    if (lo < 2) {
        lo = 2;
    }
    // Make sure the array is zeroed out.
    for (int i = 0; i < (int)arrSize; i++) {
        retArray[i] = 0;
    }

    // Mark off multiples of each prime from lo to hi.
    int nPrimes = 0;
    for (int i = 0; i < hi - lo; i++) {
        if (retArray[i] == 1) {
            continue; // skip previously marked indices
        }
        int p = lo + i; // The current prime number
        int q = 2 * p; // Some multiple of p
        while (q < hi) {
            retArray[q - lo] = 1;
            q += p;
        }
        nPrimes++;
    }

    // Fill the return array with the prime values found.
    int p = 0; // The position of the most recently printed prime.
    for (int i = 0; i < hi - lo; i++) {
        if (retArray[i] != 0) {
            continue; // Skip to the values marked as prime.
        }
        retArray[p++] = lo + i; // Store each prime in retArray.
    }
    return nPrimes;
}

#define TEST_ARR_SIZE 50
#define NUM_PRIMES_IN_TEST 15
bool testSimpleSieve()
{
    int lo = 0;
    int hi = TEST_ARR_SIZE;
    int arr[TEST_ARR_SIZE] = {0};
    int nPrimes = 0;
    bool testPassed = false;

    nPrimes = simpleSieve(&arr[0], TEST_ARR_SIZE, lo, hi);
    // Check that simpleSieve found the expected number of primes.
    testPassed = (nPrimes == NUM_PRIMES_IN_TEST) ? true : false;
    // Check that every value returned in arr is prime.
    for (int i = 0; testPassed && (i < nPrimes); i++) {
        testPassed = trialDivision(arr[i]) ? testPassed : false;
    }
    // Check that arr matches FIRST_16_PRIMES.
    for (int i = 0; testPassed && (i < NUM_PRIMES_IN_TEST); i++) {
        testPassed = arr[i] == FIRST_16_PRIMES[i] ? testPassed : false;
    }

    if (!testPassed) {
        fprintf(stderr, "testSimpleSieve failed.\n");
        fprintf(stderr, "\tFound %d primes, need %d.\n",
                nPrimes, NUM_PRIMES_IN_TEST);
        fprintf(stderr, "\tProduced the array: {%d", arr[0]);
        for (int i = 1; i < TEST_ARR_SIZE; i++) {
            fprintf(stderr, ", %d", arr[i]);
        }
        fprintf(stderr, "}\n");
    }

    return testPassed;
}
#undef NUM_PRIMES_IN_TEST
#undef TEST_ARR_SIZE

bool testSimpleSieveOnRange(int lo, int hi)
{
    size_t size = hi - lo;
    int arr[size];
    for (int i = 0; i < (int)size; i++) {
        arr[i] = 0;
    }

    int nPrimes = simpleSieve(&arr[0], size, lo, hi);
    bool testPassed = true;
    for (int i = 0; testPassed && (i < nPrimes); i++) {
        testPassed = trialDivision(arr[i]) ? testPassed : false;
    }

    return testPassed;
}

int main (int argc, char **argv)
{
    int lo = 0;
    int hi = 0;
    int pr = 0;
    if (argc < 4
        || (sscanf(argv[1], "%d", &lo) < 1)
        || (sscanf(argv[2], "%d", &hi) < 1)
        || (sscanf(argv[3], "%d", &pr) < 1)
        ) {
        printf("Usage: %s", argv[0]);
        printf(" <inclusive_floor>");
        printf(" <exclusive_ceiling>");
        printf(" <print_num_primes>\n");

        printf("testSimpleSieve:               %s\n",
               testSimpleSieve() ? "PASS" : "FAIL");
        printf("testSimpleSieveOnRange(0,100): %s\n",
               testSimpleSieveOnRange(0,100) ? "PASS" : "FAIL");
    } else {
        size_t size = hi - lo;
        int arr[size];
        for (int i = 0; i < (int)size; i++) {
            arr[i] = 0;
        }

        int nPrimes = simpleSieve(&arr[0], size, lo, hi);
        printf("Found %d primes.\n", nPrimes);
        int j = pr < nPrimes ? pr : nPrimes; // Number of primes to print
        for (int i = nPrimes - j; i < nPrimes; i++) {
            printf("%d ", arr[i]);
        }
        printf("\n");
    }

    return 0;
}
