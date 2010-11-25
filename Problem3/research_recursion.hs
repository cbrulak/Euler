import System.Time
import Control.Parallel
import System( getArgs )
import Char
import List


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = 
	fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

mkList :: Int -> [Int]
mkList n = [1..n-1]

relprime :: Int -> Int -> Bool
relprime x y = gcd x y == 1

euler :: Int -> Int
euler n = length (filter (relprime n) (mkList n))

seqSumEuler :: Int -> Int
seqSumEuler = sum . (map euler) . mkList

parSumEuler :: Int -> Int
parSumEuler = sum . (map euler) . mkList

seqSumFibEuler:: Int -> Int -> Int
seqSumFibEuler a b = fib a + seqSumEuler b

parSumFibEuler a b = f `par` (e `par` (e+ f)) 
	where  
		f = fib a 
		e = parSumEuler b	

modArray :: Int -> Int -> [Int]
modArray a b =  [ x | x <- [1..a], x `mod` b == 0] 

modArrayFactorization :: Int -> [Int]
modArrayFactorization a = f 
	where
		allNumbers =  mkList (a)
		f = mkList a
		e = mkList a

--deleteDups :: [Int] -> [Int] -> [Int]
--deleteDups [n] [] = []
--deleteDups [n] [m] = delete ((head n) (deleteDups m))

isPrime :: Int -> Int -> Bool
isPrime x y = y `mod` x /= 0

isPrimeRec :: [Int] -> Int -> Bool
--isPrimeRec x y = isPrime x y 
isPrimeRec x y = 
	--	putStrLn ("allNumbers is  : " ++ show x)
	if(length x == 0)
		then True
	else if (head(x) == y)
		then True
	else if (head(x) == 1) 
		then isPrimeRec (tail(x)) y
	else if (y `mod` (head(x)) /= 0) 
		then isPrimeRec (tail(x)) y
	else False

isAFactor :: Int -> Int -> Bool	
isAFactor x y = x `mod` y == 0
	
generatePrimeFactors :: Int -> [Int]	
generatePrimeFactors num = 
	let allNumbers =  mkList (num + 1)
	    allFactors =  mkList (num + 1)
	    mainList2 = filter (isPrimeRec allFactors) allNumbers
	in
		filter (isAFactor num) mainList2
	
main :: IO ()
main = 
	do 
		args<-getArgs
		
		--boundary <- 
	
		t0 <- getClockTime
		let first = (read (head args))
		
		let factors = generatePrimeFactors first
		putStrLn ("factorList is  : " ++ show factors)
		--let second =(read (head(tail args)))
		
		--putStrLn ("first arg is : " ++ show first)
		--putStrLn ("second arg is : " ++ show second)
		
		--let allNumbers =  mkList (first + 1)
		--let allFactors =  mkList (first + 1)
		--putStrLn ("allNumbers is  : " ++ show allNumbers)
		
		--let primeFactors = modArray first 2
		--putStrLn ("even factors are : " ++ show primeFactors)
		
		--let mainList2 =  allNumbers \\ primeFactors
		--let mainList2 = map deleteNonPrimes allNumbers
				
		--let mainList2 = filter (isPrimeRec allFactors) allNumbers
		--putStrLn ("mainList is  : " ++ show mainList2)
		
		--let factorList = filter (isAFactor first) mainList2
		--putStrLn ("factorList is  : " ++ show factorList)
		
		
		--let r1 = seqSumFibEuler first second
		--let r2 = parSumFibEuler first second
		
		
		
		--pseq r1 (return())
		t1 <- getClockTime
		--putStrLn ("seq sum: " ++ show r1)
		putStrLn ("time: " ++ show (secDiff t0 t1) ++ " seconds.")
		t0 <- getClockTime
		--pseq r2 (return())
		t1 <- getClockTime
		--putStrLn ("par sum: " ++ show r2)
		putStrLn ("par time: " ++ show (secDiff t0 t1) ++ " seconds.")