# The prime factors of 13195 are 5, 7, 13 and 29.
# What is the largest prime factor of the number 600851475143 ?
#
# http://projecteuler.net/index.php?section=problems&id=3
#

import sys

def isPrime(num,lastPrime):
	bIsPrime = True
	if(lastPrime == 1):
		lastPrime = 2
	if(num == 2):
		return True
	if(num == 3):
		return True
	if(num == 5):
		return True
	if(num == 7):
		return True
	if(num % 2 == 0):
		return False
	if(num % 3 == 0):
		return False
	if(num % 4 == 0):
		return False
	if(num % 5 == 0):
		return False
	if(num % 6 == 0):
		return False
	if(num % 7 == 0):
		return False
	if(num % 8 == 0):
		return False
	if(num % 9 == 0):
		return False
	if(num % 10 == 0):
		return False
	#print "looking for the primes between",lastPrime,"and ",num
	for i in range (lastPrime,num):
		if(num % 2 == 0):
			bIsPrime = False
			break;
		elif(num % 3 == 0):
			bIsPrime = False
			break;
		elif(num % 4 == 0):
			bIsPrime = False
			break;
		elif(num % 5 == 0):
			bIsPrime = False
			break;
		elif(num % 6 == 0):
			bIsPrime = False
			break;
		elif(num % 7 == 0):
			bIsPrime = False
			break;
		elif(num % 8 == 0):
			bIsPrime = False
			break;
		elif(num % 9 == 0):
			bIsPrime = False
			break;
		elif(num % 10 == 0):
			bIsPrime = False
			break;
		else:
			if( (num%i) == 0 & i != num):
				bIsPrime = False
			break;
	return bIsPrime



numToBeFactored = 13195

if(len(sys.argv) > 1):
	numToBeFactored = int(sys.argv[2])

product = 1

curPrime = 11
lastPrime = 11
primeFactors = [1,2,3,5,7]

print "Number to Factored",numToBeFactored

while(curPrime < numToBeFactored):
	#print "adding ",curPrime,"to the array"
	if((numToBeFactored%curPrime) == 0):
		print "adding ",curPrime,"to the array"
		primeFactors.append(curPrime)
		lastPrime = curPrime
	curPrime = curPrime + 1
	while(not(isPrime(curPrime,lastPrime))):
		#print curPrime,"is not a prime"
		curPrime = curPrime + 1
	

print "Prime factors are ",primeFactors



