import System.Time
import Control.Parallel
import System( getArgs )
import Char
import List

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = 
	fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

isPrime :: Integer -> Integer -> Bool
isPrime x y = y `mod` x /= 0

isPrimeRec_Primitives :: Integer -> Integer -> Bool
isPrimeRec_Primitives x y = 
	--	putStrLn ("allNumbers is  : " ++ show x)
	if(x == 1)
		then True
	else if (x == y)
		then isPrimeRec_Primitives (x-1) y
	else if (y `mod` x /= 0) 
		then isPrimeRec_Primitives (x-1) y
	else False

isPrimeRec_Array :: [Integer] -> Integer -> Bool
--isPrimeRec x y = isPrime x y 
isPrimeRec_Array x y = 
	--	putStrLn ("allNumbers is  : " ++ show x)
	if(length x == 0)
		then True
	else if (head(x) == y)
		then True
	else if (head(x) == 1) 
		then isPrimeRec_Array (tail(x)) y
	else if (y `mod` (head(x)) /= 0) 
		then isPrimeRec_Array (tail(x)) y
	else False

isAFactor :: Integer -> Integer -> Bool	
isAFactor x y = x `mod` y == 0
	
generatePrimeFactors_Array :: Integer -> [Integer]	
generatePrimeFactors_Array num = 
	let allNumbers = [1..num]
	    allFactors = [2..num]
	    mainList2 = filter (isPrimeRec_Array allFactors) allNumbers
	in
		filter (isAFactor num) mainList2
	
generatePrimeFactors_Primitives :: Integer -> Integer -> [Integer]	
generatePrimeFactors_Primitives num target = 
	if(num == 0)
		then []
	else if(isPrimeRec_Primitives num num)
		then 
			if(isAFactor target num)
				then num : (generatePrimeFactors_Primitives(num - 1) target)
			else
				generatePrimeFactors_Primitives(num - 1) target
	else
		generatePrimeFactors_Primitives(num - 1) target

main :: IO ()
main = 
	do 
		args<-getArgs
	
		t0 <- getClockTime
		let first = (read (head args))
		
		
		let factors = generatePrimeFactors_Primitives first first
		putStrLn ("factorList is  : " ++ show factors)
		t1 <- getClockTime
		--putStrLn ("seq sum: " ++ show r1)
		putStrLn ("time: " ++ show (secDiff t0 t1) ++ " seconds.")
		t0 <- getClockTime
		--pseq r2 (return())
		t1 <- getClockTime
		--putStrLn ("par sum: " ++ show r2)
		putStrLn ("par time: " ++ show (secDiff t0 t1) ++ " seconds.")