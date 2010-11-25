import sys

print "Starting Euler Problem 6"
print "Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum."
print "See http://projecteuler.net/index.php?section=problems&id=6"

sum = 0

if(len(sys.argv) > 1): 
	if (sys.argv[2] != ""):
		bound = int(sys.argv[2])
else:	
	bound = 100
	
sumOfTheSquares = 0
squareOfTheSums = 0

for i in range (0,bound+1):
	sumOfTheSquares = i*i + sumOfTheSquares
	squareOfTheSums = squareOfTheSums + i
	
squareOfTheSums = squareOfTheSums * squareOfTheSums

print "The Sum of Squares is ", sumOfTheSquares
print "The Square of the Sums is ",squareOfTheSums	
print "The difference is ",squareOfTheSums - sumOfTheSquares 	


