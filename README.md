#Solving Coding Interview Questions with Haskell#

###Question 1: Array Order Search###

######Question######
Given an array of integers A, please find three indices i, j, k, such that i<j<k and A[i]<A[j]<A[k]. Assume that such indices exist.

######Attempt 1######
We start with our naive implementation below. We can iterate about all triples until we find a valid (i, j, k). In Haskell we don't use for loops, so 3 nested loops won't work. We could do nested folds though, but that would look pretty bad. The best way to do this is with a list comprehension. This will give us a list of all of valid triples. We then select the first one since we are guaranteed that there will be an answer. Do note, that since we are doing this on 3 variables about a list of size n that the complexity of this solution will be O(n^3) with our space complexity being O(1). This is far from ideal.

```haskell
findIndicesNaive :: Vector Int -> (Int, Int, Int)
findIndicesNaive a = [(i, j, k) | i <- indices, j <- indices, k <- indices, i < j, j < k,
                                        (a ! i) < (a ! j), (a ! j) < (a ! k) ] !! 0
    where indices = [0..(V.length a)-1]
```

######Attempt 2######
We don't need to check all possible pairs. Instead we could iterate through our list. Let our position at any given step in this iteration correspond to the index j. At any given index j, we can just search to our left for an index i that meets the conditions and an index k to the right that meets the conditions. This solution would ultimately be O(n^2) because within our initial pass through the array, we make an additional pass each time between searching for a valid i and k.

```haskell
findIndicesBetter :: Vector Int -> (Int, Int, Int)
findIndicesBetter a = M.fromJust ((L.filter (\ijk -> ijk /= Nothing) maybeIJKs) !! 0)
    where indices   = [0..(V.length a)-1]
          maybeIJKs = L.map (findIndicesForJ a indices) indices

findIndicesForJ :: Vector Int -> [Int] -> Int -> Maybe (Int, Int, Int)
findIndicesForJ a indices j
    | (not (L.null foundIs)) && (not (L.null foundKs)) = Just (foundIs !! 0, j, foundKs !! 0)
    | otherwise                                        = Nothing
    where foundIs = findI a indices j
          foundKs = findK a indices j

findI :: Vector Int -> [Int] -> Int -> [Int]
findI a indices j = L.filter (\i -> (a ! i) < (a ! j)) is
    where is = L.take j indices

findK :: Vector Int -> [Int] -> Int -> [Int]
findK a indices j = L.filter (\k -> (a ! j) < (a ! k)) ks
    where ks = L.drop (j + 1) indices
```

######Attempt 3######
We can do even better though. For each A[j] we need to know the smallest value to the left of j, as well as the largest value to the right of j. Let us create arrays B and C where B[j] is the index of the smallest element to the left of j in A and C[j] is the index of the largest element to the right of j. We can build these two arrays in two separate passes of A. We simply iterate and keep a running log of the index of the smallest (or largest) element to the left and update it as we iterate. Finally, we can now iterate once through A and check at each index if we have a valid i or k in B and C as we go. We never make any nested passes and so our solution is O(n).

```haskell
findIndicesBest :: Vector Int -> (Int, Int, Int)
findIndicesBest a = L.foldl getIJK (-1,-1,-1) indices
    where indices         = [0..(V.length a)-1]
          b               = buildB a 0 0
          c               = buildC a (V.length a - 1) (V.length a - 1)
          isValidIJK j    = ((b ! j < a ! j) && (a ! j < c ! j))
          getIJK curIJK j = if (isValidIJK j) then (b ! j, j, c ! j) else curIJK

-- We need to return a Vector instead of a list for constant time access
buildB :: Vector Int -> Int -> Int -> Vector Int
buildB a lowestIndex curIndex = V.fromList (buildB' a lowestIndex curIndex)
    where buildB' a lowestIndex curIndex
            | curIndex == V.length a - 1 = [smallerIndex]
            | otherwise                  = smallerIndex : (buildB' a smallerIndex (curIndex + 1))
            where smallerIndex = if (a ! lowestIndex) < (a ! curIndex)
                                 then lowestIndex
                                 else curIndex

-- We need to reverse our result since we built our list backwards to take advantage of constant
-- time prepend.
buildC :: Vector Int -> Int -> Int -> Vector Int
buildC a highestIndex curIndex = V.fromList (L.reverse (buildC' a highestIndex curIndex))
    where buildC' a highestIndex curIndex
            | curIndex == 0 = [largerIndex]
            | otherwise     = largerIndex : (buildC' a largerIndex (curIndex - 1))
            where largerIndex = if (a ! highestIndex) > (a ! curIndex)
                                then highestIndex
                                else curIndex
```

###Quesiton 2: Index Value Matching###

######Question######
Integers in an array are unique and increasingly sorted. Please write a function/method to find an integer from the array who is equal to its index. For example, in the array [-3, -1, 1, 3, 5], the number 3 equals its index 3. Assume that any given array has such an integer.

######Notes######
This is a problem involving a sorted list, which is exactly when a Binary Search is useful. So, that is the method (and happens to be the optimal method) that we will use here.

Note that this is an interesting problem to solve in Haskell. The basic way of doing it is binary search. Many people incorrectly conflate lists and arrays, but they are very different. One might think that the input into such a function here would be [int], but that is not true. In Haskell that is a list. If we want an array in Haskell we must use Data.Array or Data.Vector. Here, we will use Data.Vector due to its more intuitive and superior API. Note that for ease of use since lists are the typical data structure in Haskell, Data.Vector has a very nice method called fromList that builds a Vector out of a list.

All of that aside, a solution that used a list instead of an array or vector wouldn't be as efficient. Any list based solution will not necessarily be O(log n) because slicing and indexing (indexing is what matters here) with lists is not O(1) like they are with arrays or vectors.

######Attempt 1######
We could complete this in O(n) by just iterating through the list and checking if the current index equals the value at that position.

```haskell
getIntWithMatchingIndex :: Vector Int -> Int
getIntWithMatchingIndex xs = fst (V.foldl matchesIndex (-1,-1) xs)
    where matchesIndex (prevValue, prevIndex) curValue
            | curIndex == curValue = (curValue, curIndex)
            | otherwise            = (prevValue, curIndex)
            where curIndex = prevIndex + 1
```

######Attempt 2######
We can do better, as hinted in the NOTES above. We already have a sorted list and this should tip us off to the fact that we should do a binary search. That is what we will implement here. It has time complexity O(log n).

```haskell
getIntWithMatchingIndexBest :: Vector Int -> Int
getIntWithMatchingIndexBest xs = getIntWithMatchingIndexBest' xs (V.head xs) (V.last xs)

-- Helper function that essentially does a binary search
getIntWithMatchingIndexBest' :: Vector Int -> Int -> Int -> Int
getIntWithMatchingIndexBest' xs left right
    | val == mid = val
    | val > mid  = getIntWithMatchingIndexBest' xs left (mid-1)
    | val < mid  = getIntWithMatchingIndexBest' xs (mid+1) right
    where mid = left + ((right - left) div 2) -- You will have to add backticks before and after div
          val = xs ! mid
```

###Question 3: Maximal Gifts###

######Question
A board has n*m cells, and there is a gift with some value (value is greater than 0) in every cell. You can get gifts starting from the top-left cell, and move right or down in each step, and finally reach the cell at the bottom-right cell. What’s the maximal value of gifts you can get from the board?<br>
| 1|10| 3| 8|<br>
|12| 2| 9| 6|<br>
| 5| 7| 4|11|<br>
| 3| 7|16| 5|<br>
For example, the maximal value of gift from the board above is 53 (1+12+5+7+7+16+5).

######Attempt 1
We can prove a strategy for a dynamic programming problem using a bit of recursive thinking. Let f(i,j) be a function telling us the maximum value having reached cell (i,j). By simple reasoning, we know that f(i,j)=max(f(i-1,j), f(i,j-1)) + gift(i,j). That is, the maximal value at (i,j) will be made up of the current gift at space (i,j) plus either the space directly above or the space directly to the left (whichever one has a higher current maximum, as f is defined). We have now just done our recursive step. Consider the base case f(0,0). Clearly, we just pick the gift at that space since there are no previous spaces. We now have a base case and our inductive step. We combine this with our inductive hypothesis (strong induction), which verifies our inductive step since we can automatically assume f(i-1,j) and f(i,j-1) themselves have produced their maximal value. We now have a proof that by following the step outlined in our inductive proof, we will always end up with the maximal answer. Let us consider an implementation based on our recursive algorithm. We would start at (0,0). We would then make a call to (1,0) and (0,1), each with the value of our gift at (0,0) as the current 'gift score'. Those recursive calls would then make calls to (2,0), (1,1) and (0,2), (1,1) respectively. It is easy to see that this blows up into exponential time complexity. Nonetheless, let's go ahead and implement it.

```haskell
getMaximalGifts :: (Array (Int, Int) Int) -> Int
getMaximalGifts board = getGifts (0,0)
    where getGifts (i,j)
            | (i,j) == maxBounds = gifts
            | i >= fst maxBounds =  (getGifts (i,j+1)) + gifts
            | j >= snd maxBounds =  (getGifts (i+1,j)) + gifts
            | otherwise = maximum [ (getGifts (i+1,j)) + gifts
                                  , (getGifts (i,j+1)) + gifts
                                  ]
            where gifts = (board ! (i,j))
          maxBounds = snd (A.bounds board)
```

######Attempt 2
The reason the computational copmlexity for the last problem was so bad is because we were double counting. We can solve this with dynamic programming.If we store our results at each step, then we can compute at a maximum n*m things. This bounds our complexity to O(n*m), which is much better than exponential. This is a bit tricky in Haskell since most implementations of dynamic programming require modifying the state of an array, which we cannot do in Haskell. Luckily, Haskell is lazily-evaluated. This means we can still use dynamic programming, despite the stateless nature of Haskell. We create an array of a bunch of calls to our getGifts method. Since Haskell is lazy, none of these calls will actually be evaluated until they need to be and once they are their results will be kept around. Therefore, our results will only be computed and retrieved as needed, thus giving us the benefit of dynamic programming. This means the number of calls will be bounded by the size of the table, rendering our algorithm O(n^2). 

```haskell
getMaximalGiftsBest :: (Array (Int, Int) Int) -> Int
getMaximalGiftsBest board = getGifts (0,0)
    where getGifts (i,j)
            | (i,j) == maxBounds = gifts
            | i >= fst maxBounds =  (giftsArray ! (i,j+1)) + gifts
            | j >= snd maxBounds =  (giftsArray ! (i+1,j)) + gifts
            | otherwise = maximum [ (giftsArray ! (i+1,j)) + gifts
                                  , (giftsArray ! (i,j+1)) + gifts
                                  ]
            where gifts = (board ! (i,j))
          maxBounds = snd (A.bounds board)
          giftsArray = A.listArray (A.bounds board)
            [getGifts (i,j) | (i,j) <- A.range (A.bounds board)]
```

To see some examples of how both work... Try the following:<br>
> let array = A.listArray ((0,0),(3,3)) [1,10,3,8,12,2,9,6,5,7,4,11,3,7,16,5]<br>
> getMaximalGifts array<br>
> getMaximalGiftsBest array<br>
They both perform just fine. However, if you try something such as the below, you will see that the first method times out.<br>
> let array = A.listArray ((0,0),(15,15)) [4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]<br>
> getMaximalGiftsBest array<br>
> getMaximalGifts array<br>
