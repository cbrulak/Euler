import System.Time
import Control.Parallel
import System( getArgs )
import Debug.Trace
import List


secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = 
	fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

getNumOfDigits :: Int  -> Int	
getNumOfDigits testNum = 
	if(testNum <= 0)
		then 0
	else if( (div testNum 10) < 1)
		then 1
	else if  ((div testNum 100) < 1)
		then 2
	else if ((div testNum 1000) < 1)
		then 3
	else if ((div testNum 10000) < 1)
		then 4
	else if ((div testNum 100000) < 1)
		then 5
	else if ((div testNum 1000000) < 1)
		then 6
	else
		7
		
getDigit :: Int -> Int -> Int		
getDigit testNum digit = 
	if(digit <= 1)
		then testNum - (div testNum 10)*10
	else if ((getNumOfDigits testNum) > digit)
		then  testNum - ((div testNum 10^(digit-1))*(10^(digit-1)))
	else
		(div testNum (10^(digit-1)))

chopNumber :: Int -> Int
chopNumber x = 		
	let numOfDigits = getNumOfDigits x
	in 
		(div (x - (getDigit x numOfDigits)*10^(numOfDigits-1)) 10)	
		
isPalindromic :: Int -> Int -> Bool
isPalindromic testNum numOfDigits = 
	let actualNumOfDigits = getNumOfDigits(testNum)
	in
	if (testNum <= 0 || numOfDigits <= 0)
		then True
	else if(numOfDigits == 1)
		then True
	else if (numOfDigits == 2 && actualNumOfDigits == 2)
		then (getDigit testNum 1) == (getDigit testNum 2)
	else if (numOfDigits == 3 && actualNumOfDigits == 3)
		then (getDigit testNum 1) == (getDigit testNum 3)
	else if ((numOfDigits == actualNumOfDigits +1) && ((getDigit testNum 1) == 0))
		then isPalindromic (div testNum 10) (numOfDigits - 2)
	else 
		((getDigit testNum 1) == (getDigit testNum numOfDigits)) && isPalindromic (chopNumber testNum) (numOfDigits - 2)
		
isPalindrom :: Int->Bool	
isPalindrom testNum = isPalindromic testNum (getNumOfDigits testNum)

--trace ("isProductOfNumFactors_rec: Less than testNum is " ++ show testNum ++ " testFactor is " ++ show testFactor ++ " numOfdigits is " ++ show numOfDigits)
--trace ("isProductOfNumFactors_rec: It is Factor testNum is " ++ show testNum ++ " testFactor is " ++ show testFactor ++ " numOfdigits is " ++ show numOfDigits) 

isProductOfNumFactors_rec :: Int -> Int -> Int -> Bool
isProductOfNumFactors_rec testNum testFactor numOfDigits= 
	let quotient = div testNum testFactor
	in
	if((getNumOfDigits testFactor) < numOfDigits)
		then  False
	else if((testNum `mod` testFactor) == 0 && ((getNumOfDigits quotient) == numOfDigits))
		then True
	else 
		isProductOfNumFactors_rec testNum (testFactor-1) numOfDigits

isProductOfNumFactors :: Int -> Int -> Bool
isProductOfNumFactors numFactors testNum= 
	if(numFactors == 1)
		then isProductOfNumFactors_rec testNum 9 1
	else if(numFactors == 2)
		then isProductOfNumFactors_rec testNum 99 2
	else if(numFactors == 3)
		then isProductOfNumFactors_rec testNum 999 3
	else
		isProductOfNumFactors_rec testNum 9999 4
		
findLargestPalindromFactor :: Int -> [Int]
findLargestPalindromFactor factors = 
	if(factors == 1)
		then filter (isProductOfNumFactors factors) (filter isPalindrom [1*1..9*9])
	else if(factors == 2)
		then filter (isProductOfNumFactors factors) (filter (isPalindrom) [11*11..99*99])
	else if(factors == 3)
		then filter (isProductOfNumFactors factors) (filter (isPalindrom) [111*111..999*999])
	else
		filter (isProductOfNumFactors factors) (filter (isPalindrom) [1111*111..9999*9999])

main :: IO ()
main = 
	do 
		args<-getArgs
		t0 <- getClockTime
		
		let numOfDigits = (read (head args))
		
		let palindrome = findLargestPalindromFactor numOfDigits
		putStrLn ("Palindrom for  " ++ show numOfDigits ++ " num of digits is " ++ show palindrome )
		t1 <- getClockTime
		putStrLn ("time: " ++ show (secDiff t0 t1) ++ " seconds.")
	