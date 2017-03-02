{- | Jonathan Thomas
     Haskell Project 1 
     March 3, 2017
| -}


{- | range takes a list of integers and returns the range.
     range calculated by subtracting max and min of list | -}
range :: Real a => [a] -> a
range list = 
    if null list
    	then 0
    else
    	subtract (minimum list) (maximum list)


{- | 
	sorted takes a list input and returns 
     true if the list is sorted, false otherwise 
| -}
     
sorted :: Ord a => [a] -> Bool
sorted list =
    if null list || length list == 1
        then True
    else 
        if (head list) <= head (tail list)
            then sorted(tail list)
    else 
        False
        
{- | nprime takes an integer input, n, and returns the nth prime number.
primes start at 2, so nprime 1 returns 2, nprime 2 returns 3, and so on.
nprime 0 throws exception and prints message "no negative numbers allowed"
| -}

nprime :: Integral a => Int -> a
nprime n =
	if (n <= 0)
		then error "no negatives allowed"
	else
		last (take n [i | i <- [2..], isPrime i])
	
{- |
	isPrime is a helper method for nprime.
	it returns true if the integer passed in is prime
	and false if it's not prime
| -}

isPrime :: Integral a => a -> Bool
isPrime num =
	null[x | x <- [2..num - 1], num `mod` x == 0]
	
		
{- |bills takes a natural number input and 
	returns the necessary number of 20, 10, 5, and 1
	dollar bills that equal that amount.| -}
	
bills :: Integral a => a -> [a]
bills dollas =
	if(dollas <= 0)
		then error "no negatives allowed"
	else
		let
    	(twenties, leftover)  = dollas `divMod` 20
    	(tens, leftover1) = leftover `divMod` 10
    	(fives, leftover2) = leftover1 `divMod` 5
    	(ones, leftover3) = leftover2 `divMod` 1
  		
  		in
    		[twenties,tens,fives,ones]  