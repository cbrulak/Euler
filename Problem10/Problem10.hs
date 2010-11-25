import System.Time
import System( getArgs )
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
		
isPrime_mod :: Integer -> Bool
isPrime_mod x = isPrimeRecursive_mod  2 x


findAllPrimes :: Integer -> Integer -> Integer
findAllPrimes target sum = 
	if(target < 2)
		then sum
	else if(isPrime_mod target)
		then (findAllPrimes (target - 1) (sum + target))
	else
		(findAllPrimes (target - 1) sum)

main :: IO ()
main = 
	do 
		args<-getArgs
	
		let first = (read (head args))
				
		t0 <- getClockTime
		--let allPrimes = filter (isPrime_mod) [2..first]
		let allPrimes = findAllPrimes first 0
		t1 <- getClockTime
		
		putStrLn ("allPrimes is  : " ++ show allPrimes)
		putStrLn ("All Primes found took: " ++ show (secDiff t0 t1) ++ " seconds.")
		
		--t0 <- getClockTime
		--let allPrimeSum =  sum allPrimes
		--putStrLn ("sum is: " ++ show allPrimeSum)
		--t1 <- getClockTime
		
		--putStrLn ("time: " ++ show (secDiff t0 t1) ++ " seconds.")