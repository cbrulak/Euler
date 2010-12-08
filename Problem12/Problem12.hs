import System.Time
import Control.Parallel
import System( getArgs )
import Debug.Trace
import List


secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = 
	fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

generateTriangleNum :: Float -> Float
generateTriangleNum n =
    (n*n + n)/2

findNumOfDivisorsForTriangleNum :: Float -> Float
findNumOfDivisorsForTriangleNum numFactors = 
    generateTriangleNum numFactors
    
    
    
main :: IO ()
main = 
	do 
		args<-getArgs
		t0 <- getClockTime
		
		let numOfDivisors = (read (head args))
		
		let triangle = findNumOfDivisorsForTriangleNum numOfDivisors
		putStrLn ("Triangle number with " ++ show numOfDivisors ++ " num of divisors is " ++ show triangle )
		t1 <- getClockTime
		putStrLn ("time: " ++ show (secDiff t0 t1) ++ " seconds.")
	