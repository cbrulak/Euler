
import sys

def Factorial(n): # return Fibonacci series up to n
	result = 1
	for i in range (1,n):
		result = result * i
	return result