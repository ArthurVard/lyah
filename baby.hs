doubleMe x = x * x


doubleSmallNumber x = if x > 100
						then x
						else x * 2



nextElem :: Int -> Int
nextElem n | odd n = n * 3 +1
           | otherwise = div n 2


collatzSeq :: Int -> [Int] 
collatzSeq 1 = [1]
collatzSeq n = n: (collatzSeq (nextElem n))


chainsGreater15 = length . filter (\xs->length xs > 15) $ map  collatzSeq [1..100]

