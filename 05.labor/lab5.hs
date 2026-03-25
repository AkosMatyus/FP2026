-- # 5. labor

-- I. Írjuk meg a beépített splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all függvényeket.

mysplitAt n ls
    | n <= 0 = ([], ls)
    | otherwise = seged n ls
  where
    seged 0 xs = ([], xs)
    seged _ [] = ([], [])
    seged k (x:xs) = let (ys, zs) = seged (k-1) xs in (x:ys, zs)


mynotElem _ [] = True
mynotElem e (x:xs)
    | e == x = False
    | otherwise = mynotElem e xs


myconcat [] = []
myconcat (xs:xss) = seged xs (myconcat xss)
  where
    seged [] ys     = ys
    seged (x:xs) ys = x : seged xs ys

myrepeat x = x : myrepeat x


myreplicate 0 _ = []
myreplicate n x
    | n > 0  = x : myreplicate (n-1) x
    | otherwise = []


mycycle [] = error "empty list"
mycycle xs = seged xs
  where
    seged []     = seged xs
    seged (y:ys) = y : seged ys

myiterate f x = x : myiterate f (f x)

myany _ [] = False
myany p (x:xs)
    | p x = True
    | otherwise = any p xs

myall _ [] = True
myall p (x:xs)
    | p x = all p xs
    | otherwise = False

-- II. Írjunk Haskell-függvényt, amely a foldl vagy a foldr függvényt alkalmazva

-- - implementálja a length, sum, elem, reverse, product, maximum, insert-sort, ++, map, filter függvényeket,
length' :: [a] -> Int
length' = foldr (\_ acc -> acc + 1) 0

sum' :: Num a => [a] -> a
sum' = foldl (+) 0

elem' :: Eq a => a -> [a] -> Bool
elem' e = foldr (\x acc -> acc || (x == e)) False

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: Num a => [a] -> a
product' = foldl (*) 1

maximum' :: Ord a => [a] -> a
maximum' []     = error "üres lista"
maximum' (x:xs) = foldl max x xs


insertSort ls = foldr insert [] ls
  where
    insert x []     = [x]
    insert x (y:ys)
        | x <= y    = x : y : ys
        | otherwise = y : insert x ys


xs ++ ys = foldr (:) ys xs

map' f = foldr (\x acc -> f x : acc) []

filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- - meghatározza egy lista pozitív elemeinek összegét,
pozitivOsszeg :: (Num a, Ord a) => [a] -> a
pozitivOsszeg = foldr (\x acc -> if x > 0 then x + acc else acc) 0

-- - egy lista páros elemeinek szorzatát,
parosSzorzat :: Integral a => [a] -> a
parosSzorzat = foldr (\x acc -> if even x then x * acc else acc) 1
-- - n-ig a négyzetszámokat.
negyzetek n = map' (\x -> x * x) [1 .. n]
-- - meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét: $$a_0 + x_0(a_1 + x_0(a_2 + x_0(a_3 + \ldots + x_0(a_{n-1}+ x_0 \cdot a_n))))$$

-- III.

-- - Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat, amelyek karakterszáma a legkisebb. Például ha a lista a következő szavakat tartalmazza:  function class Float higher-order monad tuple variable Maybe recursion  akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe
-- - Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.
--   Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.

--   ```haskell
--   > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
--   [2, 6, 8]
--   > talalat 'e' "Bigeri-vizeses"
--   [3,10,12]
--   ```
-- - Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
--   Például:

--   ```haskell
--   > ls = [("golya",120),("fecske",85),("cinege",132)]
--   > osszegT ls
--   337
--   ```
-- - Írjunk egy atlagTu Haskell-függvényt, amely egy kételemű, tuple elemtípusú lista esetében átlagértékeket számol a második elem szerepét betöltő listaelemeken. Az eredmény egy tuple elemtípusú lista legyen, amelynek kiíratása során a tuple-elemeket formázzuk, és külön sorba írjuk őket.
--   Például:

--   ```haskell
--   > :set +m
--   > ls = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),
--   | ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
--   > atlagTu ls
--   mari 7.375
--   feri 9.0
--   zsuzsa 7.466666666666666
--   levi 8.875
--   ```
