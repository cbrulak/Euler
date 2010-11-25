import sys

print "Starting Euler Problem 9"
print "Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000."
print "See http://projecteuler.net/index.php?section=problems&id=9"

#print sys.argv[1] , sys.argv[2]


def IsPythagoreanTriplet(a,b,c):
	if(a == 0):
		return false
	if(b == 0):
		return false
	if(c == 0):
		return false
	if(a > b):
		return false
	if(b > c):
		return false
	return (((a*a) + (b*b)) == (c*c))


maxSum = 1000

if(IsPythagoreanTriplet(2,3,4)):
	print "triplete found ",2,3,4 
if(IsPythagoreanTriplet(3,4,5)):
	print "triplete found ",2,3,4 

for i in range (2,maxSum):
	for j in range (i+1,maxSum):
		for k in range (j+1,maxSum):
			#print "looking at ",i,j,k,i+j+k
			if((i+j+k) == maxSum):
				#print "Sum found ",i,j,k
				if( IsPythagoreanTriplet(i,j,k)):
					print "triplete found ",i,j,k,"product is",i*j*k
		
print "nothing found"

