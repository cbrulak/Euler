import sys

print "Starting Euler Problem 1"
print "This problem sums up all the numbers between 0 and 1000 which are multiples of 3 or 5."
print "See http://projecteuler.net/index.php?section=problems&id=1"

#print sys.argv[1] , sys.argv[2]

bound = 0

print len(sys.argv)

if(len(sys.argv) > 1): 
	if (sys.argv[2] != ""):
		bound = int(sys.argv[2])
else:	
	bound = 1000
	
sum = 0
for i in range (0,bound):
	#print "i is ",i
	if( i % 5 == 0) | (i % 3 == 0):
		#print "i is divisble is by 3 or 5",i
		sum = sum + i

print "the boundary is",bound
print "the sum is",sum

