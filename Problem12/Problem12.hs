import System.Time
import Control.Parallel
import System( getArgs )
import Debug.Trace
import List


secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = 
	fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

divS n = [ x | x <- [1..(n)], n `rem` x == 0 ]    
    
generateTriangleNum :: Int -> Int
generateTriangleNum n =
    (n*n + n) `div` 2

findNumOfDivisorsForTriangleNum :: Int -> Int
findNumOfDivisorsForTriangleNum numFactors = 
    length (divS (generateTriangleNum( numFactors)))

findNumOfDivisorsForTriangleNumArr :: Int -> [Int]
findNumOfDivisorsForTriangleNumArr numFactors = 
    divS (generateTriangleNum( numFactors))

findNumOfFactors :: Int -> Int -> Int
findNumOfFactors numOfFactors num =
    let i = (findNumOfDivisorsForTriangleNum num)
    in
   
    if(numOfFactors == i)
        then num
    else
        trace ("findNumOfFactors: numOfFactors " ++ show numOfFactors ++ " num is " ++ show num ++ " i is " ++ show i)
        findNumOfFactors numOfFactors (num+1)
    
main :: IO ()
main = 
    do 
        args<-getArgs
        t0 <- getClockTime

        let numOfDivisors = (read (head args))

        --let triangle = findNumOfDivisorsForTriangleNum numOfDivisors
        --let triangleArr = findNumOfDivisorsForTriangleNumArr numOfDivisors
        --let triangle = length(triangleArr)

        --putStrLn ("Triangle number with " ++ show numOfDivisors ++ " num of divisors is " ++ show triangle ++ show triangleArr)
        
        let firstTriangle = findNumOfFactors numOfDivisors 2
        
        putStrLn ("Triangle number is " ++ show firstTriangle)
        
        t1 <- getClockTime
        putStrLn ("time: " ++ show (secDiff t0 t1) ++ " seconds.")
	