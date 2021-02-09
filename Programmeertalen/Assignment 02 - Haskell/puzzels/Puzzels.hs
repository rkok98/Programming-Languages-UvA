module Puzzels where

length' :: [a] -> Int
length' (x) = foldr (\_ acc -> 1 + acc) 0 x

or' :: [Bool] -> Bool
or' = foldr (||) False

elem':: Eq a => a -> [a] -> Bool
elem' x = foldr (\el y -> el == x || y) False

map' :: (a -> b) -> [a] -> [b]
map' f (x) = foldr (\x xs -> f x : xs) [] x

plusplus :: [a] -> [a] -> [a]
plusplus (x:xs) ys = x : xs ++ ys


main = do 
   putStrLn "The length of the array is:"  
   print(plusplus [1,2,3,4] [1,2,3,4])