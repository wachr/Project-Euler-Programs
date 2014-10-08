/* euler-prob-07.cpp
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

int main (int argc, char **argv)
{
    int val = 0;
    if (argc < 2 || sscanf(argv[1], "%d", &val) < 1) {
        printf("%s: Invalid command line arguments.\n", argv[0]);
        return 1;
    }
    printf("%d is prime: %s\n", val, trialDivision(val) ? "true" : "false");
    return 0;
}
