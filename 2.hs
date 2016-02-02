import Data.Vector as V

{-
    QUESTION:
    Integers in an array are unique and increasingly sorted. Please write a function/method to find
    an integer from the array who is equal to its index. For example, in the array
    [-3, -1, 1, 3, 5], the number 3 equals its index 3. Assume that any given array has such an
    integer.
-}

{-
    NOTES:
    This is a problem involving a sorted list, which is exactly when a Binary Search is useful.
    So, that is the method (and happens to be the optimal method) that we will use here.

    Note that this is an interesting problem to solve in Haskell. The basic way of doing it
    is binary search. Many people incorrectly conflate lists and arrays, but they
    are very different. One might think that the input into such a function here would be
    [int], but that is not true. In Haskell that is a list. If we want an array in Haskell
    we must use Data.Array or Data.Vector. Here, we will use Data.Vector due to its more
    intuitive and superior API. Note that for ease of use since lists are the typical
    data structure in Haskell, Data.Vector has a very nice method called fromList that
    builds a Vector out of a list.

    All of that aside, a solution that used a list instead of an array or vector wouldn't
    be as efficient. Any list based solution will not necessarily be O(log n) because
    slicing and indexing (indexing is what matters here) with lists is not O(1) like they are
    with arrays or vectors.
-}

{-
    ATTEMPT 1: We could complete this in O(n) by just iterating through the list and checking if
    the current index equals the value at that position.
-}

getIntWithMatchingIndex :: Vector Int -> Int
getIntWithMatchingIndex xs = fst (V.foldl matchesIndex (-1,-1) xs)
    where matchesIndex (prevValue, prevIndex) curValue
            | curIndex == curValue = (curValue, curIndex)
            | otherwise            = (prevValue, curIndex)
            where curIndex = prevIndex + 1

{-
    ATTEMPT 2: We can do better, as hinted in the NOTES above. We already have a sorted list and
    this should tip us off to the fact that we should do a binary search. That is what we will
    implement here. It has time complexity O(log n).
-}

getIntWithMatchingIndexBest :: Vector Int -> Int
getIntWithMatchingIndexBest xs = getIntWithMatchingIndexBest' xs (V.head xs) (V.last xs)

-- Helper function that essentially does a binary search
getIntWithMatchingIndexBest' :: Vector Int -> Int -> Int -> Int
getIntWithMatchingIndexBest' xs left right
    | val == mid = val
    | val > mid  = getIntWithMatchingIndexBest' xs left (mid-1)
    | val < mid  = getIntWithMatchingIndexBest' xs (mid+1) right
    where mid = left + ((right - left) `div` 2)
          val = xs ! mid
