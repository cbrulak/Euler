import System.Time
import Control.Parallel
import System( getArgs )


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

r1 = seqSumFibEuler 10 7450
r2 = parSumFibEuler 10 7450



main :: IO ()
main = 
	do 
		args<-getArgs
		print $ show args

		t0 <- getClockTime
		pseq r1 (return())
		t1 <- getClockTime
		putStrLn ("seq sum: " ++ show r1)
		putStrLn ("seq time: " ++ show (secDiff t0 t1) ++ " seconds.")
		t0 <- getClockTime
		pseq r2 (return())
		t1 <- getClockTime
		putStrLn ("par sum: " ++ show r2)
		putStrLn ("par time: " ++ show (secDiff t0 t1) ++ " seconds.")

