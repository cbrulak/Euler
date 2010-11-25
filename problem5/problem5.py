# 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
# What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?
#
# http://projecteuler.net/index.php?section=problems&id=5
#

import sys
import factorial


def isDivisibleByAll(num,boundary):
	bIsDivisibleByAll = 1
	for i in range (2,boundary+1):
		if( (num%i) != 0):
			#print "isDivisable: ",num," is not divisble by ",i
			bIsDivisibleByAll = 0
			break;
	return bIsDivisibleByAll

def getNonDivisor(num,boundary):
	nonDivisor = 0
	for i in range (2,boundary+1):
		if( (num%i) != 0):
			#print "isDivisable: ",num," is not divisble by ",i
			nonDivisor = i
			break;
	return nonDivisor
	
def generateSequence(boundary):
	seqArray = [2]
	for i in range (3,boundary+1):
		seqArray.append(i)
	
	return seqArray

def getFirstDivisor(boundary):
	divisor = 2
	for i in range (2,boundary+1):
		if(boundary%i == 0):
			divisor = i
			break;
	return divisor
	
	
boundary = 10

if(len(sys.argv) > 1):
	boundary = int(sys.argv[2])

product = 1

seqArray = generateSequence(boundary)
print "array is ",seqArray

factors = [];

while(isDivisibleByAll(product,boundary) == 0):
	print product," not divisible by all, from 1 to ",boundary
	
	nonDivisor = getNonDivisor(product,boundary)
	print product,"non divsior is ",nonDivisor
	
	tempSeq = generateSequence(nonDivisor)
	
	divisorFactor = getFirstDivisor(nonDivisor)
	
	product = product*divisorFactor
	factors.append(divisorFactor)	
		
print "smallest number is",product
print "factors are ",factors
