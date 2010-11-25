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
	let quotient = div num 2
	in
		if(counter >  quotient)
			then do
				putStrLn ("factorList is  : " ++ show quotient)
					[]
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
		t1 <- getClockTime
		
		let factors = findFactors 2 first
		putStrLn ("factorList is  : " ++ show factors)
		
		let allPrimes = filter (isPrime) factors
		putStrLn ("allPrimes is  : " ++ show allPrimes)
		
		
		
		
		--putStrLn ("seq sum: " ++ show r1)
		putStrLn ("time: " ++ show (secDiff t0 t1) ++ " seconds.")
		t0 <- getClockTime
		--pseq r2 (return())
		t1 <- getClockTime
		--putStrLn ("par sum: " ++ show r2)
		putStrLn ("par time: " ++ show (secDiff t0 t1) ++ " seconds.")