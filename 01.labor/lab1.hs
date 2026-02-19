import Text.XHtml (base, abbr)
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
-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
-- - az n szám faktoriálisát (3 módszer),
-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).

-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
-- - az első n négyzetszámot,
-- - az első n természetes szám köbét,
-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
-- - x hatványait adott n-ig,
-- - egy szám páros osztóinak listáját,
-- - n-ig a prímszámok listáját,
-- - n-ig az összetett számok listáját,
-- - n-ig a páratlan összetett számok listáját,
-- - az n-nél kisebb Pitágorászi számhármasokat,
-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
-- - azt a listát, ami felváltva tartalmaz True és False értékeket.

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
