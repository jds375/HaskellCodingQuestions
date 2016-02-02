import Data.Vector as V
import Data.List as L
import Data.Maybe as M

{-
    QUESTION: Given an array of integers A, please find three indices i, j, k, such that i<j<k and
    A[i]<A[j]<A[k]. Assume that such indices exist.
-}

{-
    ATTEMPT 1: We start with our naive implementation below. We can iterate about all triples until
    we find a valid (i, j, k). In Haskell we don't use for loops, so 3 nested loops won't work.
    We could do nested folds though, but that would look pretty bad. The best way to do this
    is with a list comprehension. This will give us a list of all of valid triples. We then select
    the first one since we are guaranteed that there will be an answer. Do note, that since we are
    doing this on 3 variables about a list of size n that the complexity of this solution will be
    O(n^3) with our space complexity being O(1). This is far from ideal.
-}

findIndicesNaive :: Vector Int -> (Int, Int, Int)
findIndicesNaive a = [(i, j, k) | i <- indices, j <- indices, k <- indices, i < j, j < k,
                                        (a ! i) < (a ! j), (a ! j) < (a ! k) ] !! 0
    where indices = [0..(V.length a)-1]

{-
    ATTEMPT 2: We don't need to check all possible pairs. Instead we could iterate through our list.
    Let our position at any given step in this iteration correspond to the index j. At any given
    index j, we can just search to our left for an index i that meets the conditions and an index
    k to the right that meets the conditions. This solution would ultimately be O(n^2) because
    within our initial pass through the array, we make an additional pass each time between
    searching for a valid i and k.
-}

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

{-
    ATTEMPT 3: We can do even better though. For each A[j] we need to know the smallest value to
    the left of j, as well as the largest value to the right of j. Let us create arrays B and C
    where B[j] is the index of the smallest element to the left of j in A and C[j] is the index of
    the largest element to the right of j. We can build these two arrays in two separate passes of
    A. We simply iterate and keep a running log of the index of the smallest (or largest) element to
    the left and update it as we iterate. Finally, we can now iterate once through A and check at
    each index if we have a valid i or k in B and C as we go. We never make any nested passes and
    so our solution is O(n).
-}

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
