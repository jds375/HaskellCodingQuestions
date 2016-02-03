import Data.Array as A

{-
    QUESTION:
    A board has n*m cells, and there is a gift with some value (value is greater than 0) in every
    cell. You can get gifts starting from the top-left cell, and move right or down in each step,
    and finally reach the cell at the bottom-right cell. What’s the maximal value of gifts you can
    get from the board?

    | 1|10| 3| 8|
    |12| 2| 9| 6|
    | 5| 7| 4|11|
    | 3| 7|16| 5|
    For example, the maximal value of gift from the board above is 53 (1+12+5+7+7+16+5).
-}

{-
    ATTEMPT 1: We can prove a strategy for a dynamic programming problem using a bit of recursive
    thinking. Let f(i,j) be a function telling us the maximum value having reached cell (i,j). By
    simple reasoning, we know that f(i,j)=max(f(i-1,j), f(i,j-1)) + gift(i,j). That is, the maximal
    value at (i,j) will be made up of the current gift at space (i,j) plus either the space directly
    above or the space directly to the left (whichever one has a higher current maximum, as f is
    defined). We have now just done our recursive step. Consider the base case f(0,0). Clearly, we
    just pick the gift at that space since there are no previous spaces. We now have a base case and
    our inductive step. We combine this with our inductive hypothesis (strong induction), which
    verifies our inductive step since we can automatically assume f(i-1,j) and f(i,j-1) themselves
    have produced their maximal value.

    We now have a proof that by following the step outlined in our inductive proof, we will always
    end up with the maximal answer. Let us consider an implementation based on our recursive
    algorithm. We would start at (0,0). We would then make a call to (1,0) and (0,1), each with the
    value of our gift at (0,0) as the current 'gift score'. Those recursive calls would then make
    calls to (2,0), (1,1) and (0,2), (1,1) respectively. It is easy to see that this blows up into
    exponential time complexity. Nonetheless, let's go ahead and implement it.
-}

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

{-
    ATTEMPT 2: The reason the computational copmlexity for the last problem was so bad is because we
    were double counting. We can solve this with dynamic programming.

    If we store our results at each step, then we can compute at a maximum n*m things. This bounds
    our complexity to O(n*m), which is much better than exponential.

    This is a bit tricky in Haskell since most implementations of dynamic programming require
    modifying the state of an array, which we cannot do in Haskell. Luckily, Haskell is
    lazily-evaluated. This means we can still use dynamic programming, despite the stateless nature
    of Haskell. We create an array of a bunch of calls to our getGifts method. Since Haskell is
    lazy, none of these calls will actually be evaluated until they need to be and once they are
    their results will be kept around. Therefore, our results will only be computed and retrieved
    as needed, thus giving us the benefit of dynamic programming. This means the number of calls
    will be bounded by the size of the table, rendering our algorithm O(n^2).

    To see some examples of how both work... Try the following:
    > let array = A.listArray ((0,0),(3,3)) [1,10,3,8,12,2,9,6,5,7,4,11,3,7,16,5]
    > getMaximalGifts array
    > getMaximalGiftsBest array

    They both perform just fine. However, if you try something such as the below, you will see that
    the first method times out.
    > let array = A.listArray ((0,0),(15,15)) [4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
    > getMaximalGiftsBest array
    > getMaximalGifts array
-}

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
