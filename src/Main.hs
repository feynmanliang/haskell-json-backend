{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import Data.Array
import Data.Tuple
import Prelude hiding (all, reverse, takeWhile, zip, concat, concatMap)
import Test.HUnit

main :: IO ()
main = do
   _ <- runTestTT $ TestList [ testStyle,
                               testLists,
                               testBowlingKata,
                               testLcs ]
   return ()

--------------------------------------------------------------------------------

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , tarithmetic, treverse, tzap ]


abc :: Bool -> Bool -> Bool -> Bool
abc x y z = x && (y || z)


tabc :: Test
tabc = "abc" ~: TestList [abc True False True ~?= True,
                          abc True False False ~?= False,
                          abc False True True ~?= False]


arithmetic :: ((Int, Int), Int) -> ((Int,Int), Int) -> (Int, Int, Int)
arithmetic x1 x2 =
  let a = fst (fst x1)
      b = snd (fst x1)
      c = snd x1
      d = fst (fst x2)
      e = snd (fst x2)
      f = snd x2
  in
  ((b*f) - (c*e),
  (c* d) - (a*f),
  (a*e)-(b*d))


tarithmetic :: Test
tarithmetic = "arithmetic" ~:
   TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3),
             arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]


reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

treverse :: Test
treverse = "reverse" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                                  reverse [1]     ~?= [1] ]

zap :: [a -> t] -> [a] -> [t]
zap = map2 (\f a -> (f a))

tzap :: Test
tzap = "zap" ~:
  TestList [ zap [ (+1), \n -> n - 1, (+1) ]
                   ([3, 4, 5] :: [Int]) ~?= [4,3,6],
             zap [ null, not . null ] [ [], "a" ] ~?= [True, True],
             zap [] "a" ~?=  "",
             zap [not] [] ~?= []]

--------------------------------------------------------------------------------

testLists :: Test
testLists = "testLists" ~: TestList [tintersperse, tinvert, ttakeWhile, tfind, tall, tmap2, tzip, ttranspose, tconcat]

-- The intersperse function takes an element and a list
-- and intersperses that element between the elements of the list.
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"
intersperse :: a -> [a] -> [a]
intersperse c l = case l of
  (x:y:xs) -> x:c:intersperse c (y:xs)
  [x] -> [x]
  [] -> []

tintersperse :: Test
tintersperse = "intersperse" ~: TestList [intersperse ',' "abcde" ~?= "a,b,c,d,e"]


-- invert lst returns a list with each pair reversed.
-- for example:
--   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")]
--   invert ([] :: [(Int,Char)])  returns []

--   note, you need to add a type annotation to test invert with []
--
invert :: [(a,b)] -> [(b,a)]
invert = map swap

tinvert :: Test
tinvert = "invert" ~: TestList [invert [("a",1),("a",2)]    ~?= [(1,"a"),(2,"a")],
                                invert ([] :: [(Int,Char)]) ~?= []]


-- takeWhile, applied to a predicate p and a list xs,
-- returns the longest prefix (possibly empty) of xs of elements
-- that satisfy p:
-- For example,
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) = if p x
                       then x:takeWhile p xs
                       else []

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: TestList [takeWhile (< 3) [1,2,3,4,1,2,3,4] ~?= [1,2],
                                      takeWhile (< 9) [1,2,3] ~?= [1,2,3],
                                      takeWhile (< 0) [1,2,3] ~?= []]


-- find pred lst returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a "Maybe".
-- for example:
--     find odd [0,2,3,4] returns Just 3
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x
                  then Just x
                  else find p xs

tfind :: Test
tfind = "find" ~: TestList [find odd [0,2,3,4] ~?= Just 3]


-- all pred lst returns False if any element of lst fails to satisfy
-- pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs) = p x && all p xs

tall :: Test
tall = "all" ~: TestList [all odd [1,2,3] ~?= False]


-- map2 f xs ys returns the list obtained by applying f to
-- to each pair of corresponding elements of xs and ys. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- NOTE: map2 is called zipWith in the standard library.
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f xs ys = map (uncurry f) $ zip xs ys

tmap2 :: Test
tmap2 = "map2" ~: TestList [map2 (+) [1,2,3] [4,5,6] ~?= [5, 7, 9]]

-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:
--    zip [1,2] [True] returns [(1,True)]
zip :: [a] -> [b] -> [(a,b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys


tzip :: Test
tzip = "zip" ~: TestList [zip [1,2] [True] ~?= [(1,True)]]

-- transpose  (WARNING: this one is tricky!)

-- The transpose function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is not the same behavior as the library version
-- of transpose.

-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]
--    transpose  [[1,2],[3,4,5]] returns [[1,3],[2,4]]
transpose :: [[a]] -> [[a]]
transpose m = case m of
  [] -> []
  [x] -> rowToCol x
  (x:xs) -> rowToCol x `appendCol` transpose xs
  where
    rowToCol l = case l of
      [] -> []
      (y:ys) -> [y] : rowToCol ys
    appendCol = zipWith (++)


ttranspose :: Test
ttranspose = "transpose" ~: TestList [transpose [[1,2,3],[4,5,6]] ~?= [[1,4],[2,5],[3,6]],
                                      transpose  [[1,2],[3,4,5]] ~?= [[1,3],[2,4]]]

-- concat

-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
concat :: [[a]] -> [a]
concat = foldl1 (++)


tconcat :: Test
tconcat = "concat" ~: TestList [concat [[1,2,3],[4,5,6],[7,8,9]] ~?= [1,2,3,4,5,6,7,8,9]]

-- concatMap

-- Map a function over all the elements of the list and concatenate the results.
-- for example:
--    concatMap (\x -> [x,x+1,x+2]) [1,2,3]  returns [1,2,3,2,3,4,3,4,5]
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f xs = concat $ map f xs

tconcatMap :: Test
tconcatMap = "concatMap" ~: TestList [concatMap (\x -> [x,x+1,x+2]) [1,2,3] ~?= [1,2,3,2,3,4,3,4,5]]

--------------------------------------------------------------------------------

bowlingTest0 :: ([Int] -> Int) -> Test
bowlingTest0 score = "all gutter balls" ~: 0 ~=? score (replicate 20 0)

score0 :: [ Int ] -> Int
score0 _ = 0

bowlingTest1 :: ([Int] -> Int) -> Test
bowlingTest1 score =
   "allOnes" ~: 20 ~=? score (replicate 20 1)

score1 :: [ Int ] -> Int
score1 = sum

bowlingTest2 :: ([ Int ] -> Int) -> Test
bowlingTest2 score = "spare in first frame" ~: 21 ~=? score ([5, 5, 5, 1] ++ replicate 16 0)

score2 :: [ Int ] -> Int
score2 = score where
   score (x:y:z:q:xs) = if x + y == 10
                          then x + y + 2*z + q + score xs
                          else x + y + z + q + score xs
   score (x:y:xs) = x + y + score xs
   score [x] = x
   score [] = 0

score2a :: [ Int ] -> Int
score2a = score where
   score (x:y:xs) = x + y + if x + y == 10
                             then scoreSpare xs
                             else score xs
   score _ = 0
   scoreSpare (x:y:xs) = 2*x + y + if x + y == 10
                                     then scoreSpare xs
                                     else score xs
   scoreSpare _ = 0

bowlingTest3 :: ([ Int ] -> Int) -> Test
bowlingTest3 score = "strike in first frame" ~: 22 ~=? score ([10, 5, 1] ++ replicate 16 0)

score3 :: [ Int ] -> Int
score3 = score where
   score (x:y:xs)
     | x == 10 = x + scoreNext x y xs
     | otherwise = x + y + scoreNext x y xs
   score _ = 0
   scoreSpare (x:y:xs) = 2*x + y + scoreNext x y xs
   scoreSpare _ = 0
   scoreStrike (x:y:xs) = 2*x + 2*y + scoreNext x y xs
   scoreStrike _ = 0
   scoreNext x y xs
     | x == 10 = scoreStrike (y:xs)
     | x + y == 10 = scoreSpare xs
     | otherwise = score xs

bowlingTest4 :: ([ Int ] -> Int) -> Test
bowlingTest4 score = "perfect game" ~: 300 ~=? score (replicate 12 10)

score4 :: [ Int ] -> Int
score4 = score where
   score (x:y:xs)
     | x == 10 = x + scoreStrike (y:xs)
     | x + y == 10 = x + y + scoreSpare xs
     | otherwise = x + y + score xs
   score [x] = x
   score _ = 0
   scoreSpare (x:xs) = x + score (x:xs)
   scoreSpare _ = 0
   scoreStrike [y,z] = y + z -- case: striking out
   scoreStrike (x:y:xs) = x + y + score (x:y:xs)
   scoreStrike (x:xs) = x + score (x:xs)
   scoreStrike _ = 0


testBowlingKata :: Test
testBowlingKata = TestList (map checkOutput scores) where
  -- the five test cases, in order
  bowlingTests  = [bowlingTest0, bowlingTest1, bowlingTest2,
                   bowlingTest3, bowlingTest4]

  -- the names of the score functions, the functions themselves,
  -- and the expected number of passing tests
  scores = zip3 ['0' ..] [score0, score1, score2a, score3, score4] [1..]

  -- a way to run a unit test without printing output
  testSilently = performTest (\ _ _ -> return ())
                   (\ _ _ _ _ -> return ()) (\ _ _ _ _ -> return ()) ()

  -- run each bowling test on the given score function, making sure that
  -- the expected number of tests pass.
  checkOutput (name, score, pass) = " Testing score" ++ [name] ~: do
    (s0,_) <- testSilently
       (TestList $ bowlingTests `zap` replicate 5 score)
    assert $ pass @=? cases s0 - (errors s0 + failures s0)

--------------------------------------------------------------------------------

-- Slow exponential solution
lcs' :: String -> String -> String
lcs' a b = reverse $ d m n
  where
    (m, n) = (length a, length b)
    d _ 0 = ""
    d 0 _ = ""
    d i j
      | a !! (i-1) == b !! (j-1) = a !! (i-1) : d (i-1) (j-1)
      | otherwise = maximumBy length [
        d (i-1) j,
        d i (j-1)
      ]

maximumBy :: (Ord a, Foldable t) => (a1 -> a) -> t a1 -> a1
maximumBy f = foldl1 max'
  where max' x y = if f x > f y then x else y

-- Better solution with memoization array and string->array for fast indexing
-- Run-time is not exponential because ds is constructed lazily when accessed and re-used
-- across overlapping sub-problems.
lcs :: String -> String -> String
lcs a b = reverse $ d m n
  where
    (m, n) = (length a, length b)
    a' = listArray (0, m-1) a
    b' = listArray (0, n-1) b
    d _ 0 = ""
    d 0 _ = ""
    d i j
      | a' ! (i-1) == b' ! (j-1) = a' ! (i-1) : ds ! (i-1, j-1)
      | otherwise = maximumBy length [
        ds ! (i-1, j),
        ds ! (i, j-1)
      ]
    ds = listArray bds [d i j | (i, j) <- range bds]
    bds = ((0,0), (m,n))

testLcs :: Test
testLcs = "Lcs" ~: TestList [ lcs "Advanced" "Advantaged" ~?= "Advaned",
    lcs "abcd" "acbd" ~?= "acd" ]



