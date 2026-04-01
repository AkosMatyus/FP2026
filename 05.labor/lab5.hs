import System.Win32 (LOCALESIGNATURE(lsCsbDefault), kEY_SET_VALUE)
import GHC.IO.Handle.FD (openBinaryFile)
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
myLengthL ls = foldl op 0 ls
    where
        op res k = res+1

myLengthR ls = foldr op 0 ls
    where 
        op k res = res + 1

mySumL ls = foldl op 0 ls
    where
        op res k = res + k

mySumR ls = foldr op 0 ls
    where
        op k res = res + k

myElemL c ls = foldl (op c) False ls
    where 
        op c res k 
            | c == k = True
            | otherwise = res

myElemR c ls = foldr (op c) False ls
    where
        op c k res = (c == k) || res

myReverseL ls = foldl op [] ls
    where
        op res k = k : res

myReverseR ls = foldr op [] ls
    where
        -- op k res = res ++ [k]
        op k res = res <> [k]

myProductL ls = foldl op 1 ls
    where 
        op res k = res * k

myProductR ls = foldl op 1 ls
    where 
        op k res = res * k

myMaximumL ls = foldl op (head ls) ls
    where
        op res k 
            | res > k = res
            | otherwise = k

myMaximumL1 ls = foldl1 op ls --kezdoertek automatice a lista eleme lesz
    where 
        op res k
            | res > k = res
            | otherwise = k

-- erre nem alklmasak a fold muveletek
-- myIn x ls = foldr (op x) [] ls
--     where
--         op x k res
--             | x > k = k : res
--             | otherwise = x : k : res

ins :: (Ord a) => a -> [a] -> [a]
ins x [] = [x]
ins x (k : ve)
    | x > k = k : ins x ve
    | otherwise = x : k : ve

myAppend ls1 ls2 = foldr op ls2 ls1
    where 
        op k res = k : res

myMap fg ls = foldr (op fg) [] ls
    where 
        op fg k res = fg k : res

myFilter fg ls = foldr (op fg) [] ls
    where
        op fg k res
            | fg k == True = k : res
            | otherwise = res


-- - meghatározza egy lista pozitív elemeinek összegét,
mySumPos ls = foldl op 0
    where 
        op res k
            | k > 0 = res + k
            | otherwise = res

-- - egy lista páros elemeinek szorzatát,
myProduct ls = foldl op 1 ls
    where 
        op res k 
            -- | mod k 2 == 0 = res * k
            | even k = res * k
            | otherwise = res

-- - n-ig a négyzetszámokat.
negyzetszam n = foldr op [] [1..n]
    where
        op k res 
            | k * k < n = k * k : res
            | otherwise = res

-- - meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét: $$a_0 + x_0(a_1 + x_0(a_2 + x_0(a_3 + \ldots + x_0(a_{n-1}+ x_0 \cdot a_n))))$$
polinom x0 ls = foldr (op x0) 0 ls
    where 
        op x0 k res = k+x0*res

-- III.

-- - Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat, amelyek karakterszáma a legkisebb. Például ha a lista a következő szavakat tartalmazza:  function class Float higher-order monad tuple variable Maybe recursion  akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe
legrovidebbek [] = []
legrovidebbek xs = [s | s <- xs, length s == minHossz]
  where
    minHossz = minimum (map length xs)

-- - Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.
--   Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.

--   ```haskell
--   > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
--   [2, 6, 8]
--   > talalat 'e' "Bigeri-vizeses"
--   [3,10,12]
--   ```
talalat x xs = [i | (i, e) <- zip [0..] xs, e == x]

-- - Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
--   Például:

--   ```haskell
--   > ls = [("golya",120),("fecske",85),("cinege",132)]
--   > osszegT ls
--   337
--   ```
osszegT xs = sum [n | (_, n) <- xs]


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

atlagTu xs = mapM_ kiir xs
  where
    kiir (nev, szamok) = putStrLn (nev ++ " " ++ show (atlag szamok))
    atlag ys = sum ys / fromIntegral (length ys)