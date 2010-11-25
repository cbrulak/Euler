module Data.Time.Clock.POSIX
(
	posixDayLength,POSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds,getPOSIXTime
) where


--import Data.Time.Clock.UTC
--import Data.Time.Calendar.Days
import Data.Fixed
import Control.Monad
--import System.Time 
import Data.Word	( Word64)
import System.Win32.Time

--import Data.Time.Clock.POSIX
import Data.Maybe
import Numeric
import Char
import Control.Parallel.Strategies

-- | 86400 nominal seconds in every day
posixDayLength :: NominalDiffTime
posixDayLength = 86400

-- | POSIX time is the nominal time since 1970-01-01 00:00 UTC
-- 
-- To convert from a 'Foreign.C.CTime' or 'System.Posix.EpochTime', use 'realToFrac'.
--
type POSIXTime = NominalDiffTime

 
 
getPOSIXTime :: IO POSIXTime
getPOSIXTime = do
  FILETIME ft <- System.Win32.Time.getSystemTimeAsFileTime
  return (fromIntegral (ft - win32_epoch_adjust) / 10000000)

win32_epoch_adjust :: Word64
win32_epoch_adjust = 116444736000000000

 
-- binary representation of an integer
binary :: Integer -> String 
binary = flip (showIntAtBase 2 intToDigit) []
 
-- returns True if (a) is a witness that odd n is compound
witness :: Integer -> Integer -> Bool
witness n a = pow (tail $ binary $ n-1) a
  where
    pow _ 1 = False
    pow [] _ = True
    pow xs d = pow' xs d $ (d*d) `mod` n
      where
        pow' _ d d2 | d2 == 1 && d /= (n-1) = True
        pow' ('0':xs) d d2 = pow xs d2
        pow' ('1':xs) d d2 = pow xs $ (d2*a) `mod` n
 
-- is (n) a prime with probability 4^(-k)
miller_rabin k n
  | n `mod` 2 == 0 = n == 2
  | otherwise = not $ any (witness n) $ takeWhile (< n) $ take k primes
 
primes :: [Integer]
primes = 2:3:5: [ x | x <- candidates 7 11, isPrime x ]
    where
      candidates a1 a2 = a1 : a2 : candidates (a1+6) (a2+6)
 
-- simple primality test applied for indices only, not for Fibonacci numbers      
isPrime x = all ((0 /=) . (x `mod`)) $ takeWhile ((x>=).(^2)) primes
 
fib = 1 : 1 : [ a+b | (a,b) <- zip fib (tail fib) ]
 
-- indexed Fibonacci numbers
numfib = zip [1..] fib
 
isPrimeOr4 x = x /= 1 && x /=2 && (x == 4 || isPrime x)
 
-- Fibonacci numbers with primal indices
numfib' = filter (isPrimeOr4 . fst) numfib
 
isProbablyPrime = miller_rabin 10
 
maybeFibprimes = [if isProbablyPrime f then Just (n,f) else Nothing | (n,f) <- numfib' ]
 
-- probably you need to increase 10 if running the program
-- in more than two threads
fibprimes = catMaybes $ parBuffer 10 rnf maybeFibprimes

main :: IO () 
main = do
  start <- getPOSIXTime
  printEach fibprimes start 1
    where printEach (x:xs) start n = do
          t <- getPOSIXTime
          print (t - start, n, fst x)
          printEach xs start (n+1)