import System.Time
import Control.Parallel
import System( getArgs )
import Char
import List

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = 
	fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

isPrimeRecursive_mod :: Integer -> Integer -> Bool
isPrimeRecursive_mod  factor numberIsPrime = 
	if(factor >= numberIsPrime)
		then True
	else if(numberIsPrime `mod` factor == 0)
		then False
	else
		isPrimeRecursive_mod (factor + 1) numberIsPrime
		
isPrime :: Integer -> Bool
isPrime x = isPrimeRecursive_mod  2 x

findNthPrime :: Integer -> Integer -> Integer -> Integer
findNthPrime lastPrime lastPrimeNum nthPrime = 
	if(lastPrimeNum == nthPrime)
		then lastPrime
	else if (isPrime (lastPrime + 1))
		then findNthPrime (lastPrime + 1) (lastPrimeNum+1) nthPrime
	else
		findNthPrime (lastPrime + 1) lastPrimeNum nthPrime
		

main :: IO ()
main = 
	do 
		args<-getArgs
	
		t0 <- getClockTime
		let nthPrime = (read (head args))
		
		
		let prime = findNthPrime 2 1 nthPrime
		putStrLn ("nthPrime is  : " ++ show prime)
		t1 <- getClockTime
		putStrLn ("time: " ++ show (secDiff t0 t1) ++ " seconds.")
		
		