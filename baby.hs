lucky :: (Integral a) => a -> String

lucky 7 = "Lucky number seven!"
lucky x = "Sorry, you're out of lucky!"

factorial :: (Integral a) => a -> a

factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors:: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' xs = case xs of [] -> error "Can't call head on an empty list, dummy!"
                      (x:_) -> x


bmiTell :: (RealFloat a) => a -> a -> String

bmiTell weight height
      | bmi <= skinny = "You're underweight."
      | bmi <= normal = "You're normal."
      | bmi <= fat    = "You're fat."
      | otherwise     = "You're a whale."
      where bmi = weight / height ^ 2
            (skinny, normal, fat) = (18.5, 25.0, 30.0)


initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

cylinder :: (RealFloat a) => a -> a -> a

cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea  = pi * r ^ 2
  in sideArea + 2 * topArea


calcBims :: (RealFloat a) => [(a, a)] -> [a]
calcBims xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
      | x > maxTail = x
      | otherwise = maxTail
      where maxTail = maximum' xs


replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
          | n <= 0  = []
          | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
      | n <= 0 = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys



elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs)
      | a == x    = True
      | otherwise = a `elem'` xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted


multThree:: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x


dividedByTen :: (Floating a) => a -> a
dividedByTen  = ( / 10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f(f x)