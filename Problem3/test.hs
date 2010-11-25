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