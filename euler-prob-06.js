// euler-prob-06.js
// author: Ray Wach
// date: 2014-10-14
// info: Javascript to solve the answer for Project Euler problem 6.

var ceiling = result = NaN;

ceiling = process.argv[2]
console.log("ceiling: " + ceiling);

var sumOfSquares = squareOfSum = 0;

for (var i = 1; i <= ceiling; i++) {
    sumOfSquares += i * i;
    squareOfSum += i; // Note, this is not yet squared.
}
squareOfSum *= squareOfSum;

result = squareOfSum - sumOfSquares

console.log("sumOfSquares: " + sumOfSquares)
console.log("squareOfSum: " + squareOfSum)
console.log("result: " + result)
