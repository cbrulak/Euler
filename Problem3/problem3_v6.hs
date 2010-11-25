import System.Time
import Control.Parallel
import System( getArgs )
import Char
import List

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = 
	fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

isPrimeRecursive :: Integer -> Integer -> Integer -> Bool	
isPrimeRecursive factor multiplier numberIsPrime = 
	if(multiplier >= numberIsPrime)
		then True
	else if (factor == numberIsPrime)
		then isPrimeRecursive 1 (multiplier + 1) numberIsPrime
	else if (factor*multiplier == numberIsPrime)
		then False
	else
		isPrimeRecursive (factor + 1) multiplier numberIsPrime

	
isPrime :: Integer -> Bool
isPrime x = isPrimeRecursive 1 2 x
		
isAFactor :: Integer -> Integer -> Bool	
isAFactor x y = x `mod` y == 0


findFactors :: Integer -> Integer -> [Integer]
findFactors counter num = 
	let quotient = div num counter
	in
		if(counter >  quotient)
			then []
		else if(isAFactor num counter)
			then [counter] ++ [quotient] ++ findFactors (counter + 1) num
		else
			findFactors (counter + 1) num
		

main :: IO ()
main = 
	do 
		args<-getArgs
	
		t0 <- getClockTime
		let first = (read (head args))
		
		
		let factors = sort (findFactors 2 first)
		putStrLn ("factorList is  : " ++ show factors)
		t1 <- getClockTime
		putStrLn ("time: " ++ show (secDiff t0 t1) ++ " seconds.")
		
		t0 <- getClockTime
		let allPrimes = filter (isPrime) factors
		putStrLn ("allPrimes is  : " ++ show allPrimes)
		t1 <- getClockTime
		
		putStrLn ("time: " ++ show (secDiff t0 t1) ++ " seconds.")
		
		--putStrLn ("seq sum: " ++ show r1)
		
		
	