import sys
import fibonacci

print "Starting Euler Problem 2"
print "Find the sum of all the even-valued terms in the fibonacci sequence which do not exceed four million."
print "See http://projecteuler.net/index.php?section=problems&id=2"

sum = 0

lastTerm = 0

if(len(sys.argv) > 1): 
	if (sys.argv[2] != ""):
		bound = int(sys.argv[2])
else:	
	bound = 4000000


fibs = fibonacci.FibonacciSequenceInArray(bound)

#print fibs

for term in fibs:
	if(term % 2 ==0 ):
		sum = sum + term
		
print "Sum is",sum