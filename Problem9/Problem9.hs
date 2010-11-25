import System.Time
import Control.Parallel
import System( getArgs )
import Char
import List
import Debug.Trace

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = 
	fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

generatePythagoreanTripleA :: Int -> [Int]
generatePythagoreanTripleA n = 
	[(2*n),((n*n)-1),((n*n)+1)]
	
generatePythagoreanTripleB :: Int -> [Int]
generatePythagoreanTripleB n = 
	[((2*n) + 1),(2*n)*(n+1),((2*n)*(n+1))+1]	
	
--generatePythagoreanTriple :: Int -> [Int]
--generatePythagoreanTriple n = 
--	[(generatePythagoreanTripleB n)] ++ [(generatePythagoreanTripleA n)	]
	
isPythagoreanTriplet_list :: [Int] -> Bool	
isPythagoreanTriplet_list list = 
	if((length list) < 3)
		then False
	else if(a > b || b > c || a > c)
		then False
	else if ((a*a + b*b) /= (c*c))
		then False
	else True
	where
		a = head list
		b = head (tail list)
		c = last list

isPythagoreanTripletProduct :: Int -> [Int] -> Bool
isPythagoreanTripletProduct target list = 
	if( (isPythagoreanTriplet_list list) && ( (a + b + c) == target))
		then True
	else
		False
	where
		a = head list
		b = head (tail list)
		c = last list
		

--findPythagoreanTripletProduct :: Int -> [Int]
--findPythagoreanTripletProduct target = 
--	filter isPythagoreanTripletProduct (filter isPythagoreanTriplet_list (map generatePythagoreanTriple [1..target]))	
		
main :: IO ()
main = 
	do 
		args<-getArgs

	
		t0 <- getClockTime
		let target = (read (head args))
		
		let tripleList = target + 1
		--findPythagoreanTripletProduct target
		--let aproduct = product tripleList
		putStrLn ("maxSum is  : " ++ show tripleList)
		t1 <- getClockTime
		putStrLn ("time: " ++ show (secDiff t0 t1) ++ " seconds.")