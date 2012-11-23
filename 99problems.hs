-- http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems
-- by Jordan Scales
-- http://jordanscales.com

module My99Problems where

-- 1. (*) Find the last element of a list.
myLast :: [a] -> a
myLast [x]    = x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' = head . reverse

-- 2. (*) Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [x,_]  = x
myButLast (_:xs) = myButLast xs

myButLast' :: [a] -> a
myButLast' = head . tail . reverse

-- 3. (*) Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (_:xs) p = elementAt xs (p-1)

elementAt' :: [a] -> Int -> a
elementAt' ls i = ls !! (i-1)

-- 4. (*) Find the number of elements of a list.
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' xs = sum [1 | _ <- xs]

-- 5. (*) Reverse a list.
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = (reverse xs) ++ [x]

-- 6. (*) Find out whether a list is a palindrome. A palindrome can be read 
--        forward or backward; e.g. (x a m a x).
isPalindrone :: (Eq a) => [a] -> Bool
isPalindrone xs = xs == (myReverse xs)

-- 7. (**) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x)      = [x]
flatten (List [])     = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

flatten' :: NestedList a -> [a]
flatten' (Elem x)  = [x]
flatten' (List xs) = foldl (++) [] $ map flatten xs

-- 8. (**) Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (a:b:xs)
  | a == b    = compress (a:xs)
  | otherwise = a:compress (b:xs)

-- 9. (**) Pack consecutive duplicates of list elements into sublists. If a list 
--         contains repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack (x:xs) = packIn xs [x]
  where packIn [] b = [b]
        packIn (x:xs) b
          | x == (head b) = packIn xs (x:b)
          | otherwise     = b : packIn xs [x]

pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (takeWhile (== x) (x:xs)) : (pack' $ dropWhile (== x) xs)

-- 10. (*) Run-length encoding of a list. Use the result of problem P09 to implement 
--         the so-called run-length encoding data compression method. Consecutive duplicates of elements
--         are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\t -> (myLength t, head t)) . pack
