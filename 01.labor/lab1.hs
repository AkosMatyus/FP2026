import Text.XHtml (base, abbr, reset, name)
import System.Win32 (LOCALESIGNATURE(lsCsbDefault))
-- # 1. labor

-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg :: Int -> Int -> Int
osszeg a b = a + b

kulonbseg :: Double -> Double -> Double
kulonbseg a b = a - b

szorzat :: Int -> Int -> Int
szorzat a b = a * b

hanyados :: (Fractional a) => a -> a -> a
hanyados a b = a / b

hanyados2 :: (Integral a) => a -> a -> a
hanyados2 a b = div a b

hanyados3 :: (Integral a) => a -> a -> a
hanyados3 a b = a `div` b

osztmar a b = mod a b

osztmar2 a b = a `mod` b
-- - egy első fokú egyenlet gyökét,
elsoF a b = (-b) / a

-- - egy szám abszulút értékét,
abszolut a
    | a < 0 = -a
    | otherwise = a

abszolut2 a = if a < 0 then -a else a
-- - egy szám előjelét,
elojel n = if n < 0 then "negativ" else if n > 0 then "pozitiv" else "nulla"

elojel2 n
    | n < 0 = "negativ"
    | n > 0 = "pozitiv"
    | otherwise = "nulla"
-- - két argumentuma közül a maximumot,
max_ a b = if a > b then a else b

max2 a b
    | a > b = a
    | otherwise = b

-- - két argumentuma közül a minimumot,
min_ a b
    | a < b = a
    | otherwise = b

-- - egy másodfokú egyenlet gyökeit,
masodF a b c = if delta < 0 then error "komlex szamok" else (gy1, gy2)
    where 
        delta = b**2 - 4*a*c
        gy1 = (-b + sqrt delta) / (2*a)
        gy2 = (-b - sqrt delta) / (2*a)


masodF2 a b c
    | delta < 0 = error "komplex szamok"
    | delta == 0 = [gy1]
    | otherwise = [gy1, gy2]
    where
        delta = b**2 - 4*a*c
        gy1 = (-b + sqrt delta) / (2*a)
        gy2 = (-b - sqrt delta) / (2*a)

-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
elempar ep1 ep2 = if (a == d && b == c) || (a == c && b == d) then True else False
    where
        (a,b) = ep1
        (c,d) = ep2

elempar2 (a,b) (c,d) = (a==c && b==d) || (a==d && b==c)

--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.

-- - az n szám faktoriálisát (3 módszer),

fakt 0 = 1
fakt1 n = n * fakt1(n-1)

fakt2 n
    | n < 0 = error "neg. szam"
    | n == 0 = 1
    | otherwise = n * fakt2(n-1)

fakt3 n res
    | n < 0 = error "neg. szam"
    | n == 0 = 1
    | otherwise = fakt3(n-1)(res*n)

-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).

hatvany :: (Ord a, Floating a) => a -> a -> a
hatvany x n
    | n < 0 = error "neg. kitevo"
    | otherwise = x**n

hatvany2 :: (Num a, Integral b) => a -> b -> a
hatvany2 x n 
    | n < 0 = error "neg. kitevo"
    | otherwise = x ^ n

hatvany3 x n
    | n < 0 = error "neg. kitevo"
    | n == 0 = 1
    | otherwise = x * hatvany3 x (n-1)

-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
negyzetgyok n = [sqrt i | i <- [1 .. n]]

-- - az első n négyzetszámot,
negyzetszam n = [i ^ 2 | i <- [0 .. n]]

-- - az első n természetes szám köbét,
kob n = [i ^ 3 | i <- [0 .. n]]

-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
nemNegyzet n = [i | i <- [1..n], i /= (sqrt i ** 2)]

-- - x hatványait adott n-ig,
hatvanyX x n = [x ^ i | i <- [1..n]]

-- - egy szám páros osztóinak listáját,
parosOsztok x = [i | i <- [1..x], mod x i == 0, mod i 2 == 0]

-- - n-ig a prímszámok listáját,
osztok x = [i | i <- [1..x], mod x i == 0]

primszam x = osztok x == [1, x]

primszamN n = [i | i <- [1..n], primszam i]

primszamN2 n = [i | i <- [1..n], primszamL i]
    where
        primszamL si = osztokL si == [1,si]
        osztokL si2 = [i | i <- [1..si2], mod si2 i == 0]

-- - n-ig az összetett számok listáját,
osszetett n = [i | i <- [1..n], primszam i == False]

osszetett2 n = [i | i <- [1..n], not(primszam i)]

-- - n-ig a páratlan összetett számok listáját,
paratlanOsszetett n = [i | i <- [1..n], primszam i == False, mod i 2 /= 0]

paratlanOsszetett2 n = [i | i <- [1..n], not (primszam i), odd i]

-- - az n-nél kisebb Pitágorászi számhármasokat,
pitagorasz n = [(a, b, c) | c <- [1..n], b <- [1..c], a <- [1..b], a ^ 2 + b ^ 2 == c ^ 2]

-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
betuSzam = zip ['a' .. 'z'] [0 .. 25]

betuSzam2 = zip ['a' .. 'z'] [0 .. ]

-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
szamok = zip [0..5][5, 4 ..]

szamok2 n = zip [0..n] [n, n-1 .. 0]

szamok3 n = [(i, n-i) | i <- [0..n]]

-- - azt a listát, ami felváltva tartalmaz True és False értékeket.
truefalse n = [mod i 2 == 0 | i <- [0..n]]

tf2 n = take n ls
    where
        ls = [True, False] ++ ls

-- **Megoldott feladatok:**

-- - Határozzuk meg egy szám osztóinak listáját:

--   ```haskell
--   osztok :: Int -> [ Int ]
--   osztok n = [ i | i <- [1..n] , n `mod` i ==0]

--   > osztok 100
--   ```

-- - Határozzuk meg a következő listát: $$[(\texttt{a},0), (\texttt{b},1), \ldots, (\texttt{z}, 25)]$$:

--   ```haskell
--   import Data.Char
--   lista = [(chr(i + 97), i) | i<-[0..25]]

--   lista_ = zip ['a'..'z'] [1..26]
--   ```


main :: IO ()
main = do
    putStrLn "Masodfoku egyenlet"
    print (masodF 1 2 1)
    putStrLn ("Masodfoku egyenlet 2: " ++ show(masodF 1 2 1))
    putStrLn "Elemparok: "
    print (elempar (6,7) (7,6))
    putStrLn "Faktorialis"
    print (fakt2 5)
    print (fakt2 0)
    putStrLn "Hatvany"
    print (hatvany 2 4)
    putStrLn "Negyzetszamok"
    print (negyzetszam 10)
    putStrLn "Kob"
    print (kob 10)
