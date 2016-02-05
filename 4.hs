import Data.Vector as V
import Data.List as L

{-
    QUESTION: An integer array contains elements in (strictly) increasing order till some point and
    then (strictly) decreasing order, return the index of maximum number. The array is non-empty.
    Ex - [1,2,3,4,5,3,1] will return 4.
-}

{-
    ATTEMPT 1: We can simply iterate through our array and store the index of the largest element
    we have so far. This solution is O(n)/
-}

getIndexOfMaxNum :: Vector Int -> Int
getIndexOfMaxNum xs = L.foldl getBetterIndex 0 indices
    where indices = [1..(V.length xs)-1]
          getBetterIndex i j = if xi > xj then i else j
              where xi = xs ! i
                    xj = xs ! j

{-
    ATTEMPT 2: We can do better, however. At first it may seem that we could use a binary search
    to get O(log n) since we have a sorted list... except that we don't exactly. Our list is not
    linearly sorted in the form of a incline/decline slope. Our list is more like a hill. This
    means binary search won't work. However, we can still achieve O(log n) with ternary search.
    Let us treat our array as having three parts. We can partition it into 3 parts by selecting
    an index 's' at 1/3 of the length of our array 'A' and another index 't' at 2/3 of the length of
    our array 'A'. We can begin by comparing the value of 'A[s]' to 'A[t]'. Suppose that
    'A[s]' <= 'A[t]'. Since our array is a hill, we know that any larger value would have to be
    at the position of 's' or to the right of it since the hill increases in height from 's' to 't'.
    Now suppose that 'A[s]' > 'A[t]'. Since our array is a hill, we know that any larger value would
    have to be at position t-1 or to the left of that since the hill is going up to the left.
    Observe that we now have a way to break this problem into subproblems, which is essentially what
    binary search is. This is also representative of a divide-and-conquer problem which will likely
    yield us our desired o(log n) runtime. If we repeatedly break the problem down, then we know
    that once 's' and 't' are equal, we have hit the peak. We specify our algorithm below.

    Analyze the algorithm, we observe that each time we cut the amount of work to be done by
    1/3 as we effectively ignore 1/3 of the array for each successive call. Thus, our algorithm
    can be described by the recurrence t(n) = t(2n/3) + O(1). We can expand this and solve it, or
    more simply use the master theorem to get that this is O(log n).
-}

getIndexOfMaxNumBest :: Vector Int -> Int
getIndexOfMaxNumBest a
    | s == t   = as
    | as <= at = getIndexOfMaxNumBest (V.slice (s+1) (V.length a - (s+1)) a)
    | as > at  = getIndexOfMaxNumBest (V.slice 0 t a)
    where s = ((V.length a) - 1) `div` 3
          t = 2 * ((V.length a) - 1) `div` 3
          as = a ! s
          at = a ! t
