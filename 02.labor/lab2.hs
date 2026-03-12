import Control.Monad.Trans.Cont (reset)
import GHC.Exts.Heap (GenClosure(key))
-- # 2. labor

-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

-- - egy szám számjegyeinek szorzatát (2 módszerrel),
szjSzorzat 0 = 1
szjSzorzat n = (mod n 10) * szjSzorzat (div n 10)

szjSzorzat2 n
    | n < 0 = szjSzorzat2 (abs n)
    | div n 10 == 0 = mod n 10
    | otherwise = mod n 10 * szjSzorzat2 (div n 10)

ls1 = [324, 56, 98, 72]
szjSzorzatLs = map szjSzorzat2 ls1 

-- - egy szám számjegyeinek összegét (2 módszerrel),
szjOsszeg n
    | n < 0 = szjOsszeg (abs n)
    | div n 10 == 0 = mod n 10
    | otherwise = mod n 10 + szjOsszeg (div n 10)

szjOsszeg2 n res
    | n < 0 = szjOsszeg2 (abs n) res
    | n < 10 = res + n
    | otherwise = szjOsszeg2 (div n 10) (res + mod n 10)

szjOsszegLs ls = map szjOsszeg ls

szjOsszegLs2 ls = map(\x -> (x, (szjOsszeg x))) ls 
-- - egy szám számjegyeinek számát (2 módszerrel),

szjSzam n
    | n < 0 = szjSzam (abs n)
    | div n 10 == 0 = 1
    | otherwise = 1 + szjSzam (div n 10)

szjSzam2 n res
    | n < 0 = szjSzam2 (abs n) res
    | n < 10 = res + 1
    | otherwise = szjSzam2 (div n 10) (res + 1)

-- - egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:

--   ```haskell
--   > fugv4 577723707 7
--   35
--   ```

szjParamOssz n k
    | k >= 10 = error "nem szj"
    | n < 0 = szjParamOssz (abs n) k
    | div n 10 == 0 = if mod n 10 == k then k else 0
    | mod n 10 == k = mod n 10 + szjParamOssz (div n 10) k
    | otherwise = szjParamOssz (div n 10) k

-- - egy szám páros számjegyeinek számát,
szjParos n
    | n < 0 = szjParos (abs n)
    | div n 10 == 0 = if even (mod n 10) then 1 else 0
    | otherwise = if even (mod n 10) then 1 + szjParos (div n 10) else szjParos (div n 10)

-- - egy szám legnagyobb számjegyét,
szjMax n max
    | n < 0 = szjMax (abs n) max
    | div n 10 == 0 = if mod n 10 > max then mod n 10 else max
    | otherwise = if mod n 10 > max then szjMax (div n 10) (mod n 10) else szjMax (div n 10) max

-- - egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
--   Példák függvényhívásokra:

--   ```haskell
--   fugv 7673573 10 7 -> 3
--   fugv 1024 2 1 -> 1
--   fugv 1023 2 1 -> 10
--   fugv 345281 16 4 -> 2
--   ```

bSzamSzj n b d
    | n < 0 = error "neg. szam"
    | n < b = if n == d then 1 else 0
    | otherwise = if mod n b == d then 1 + bSzamSzj (div n b) b d else bSzamSzj (div n b) b d

-- - az 1000-ik Fibonacci számot.

fiboN n = fibo 0 1 0 n
    where
        fibo _ _ res 0 = res 
        fibo a b res n1 = fibo b res (b + res) (n1-1)

fiboSg a b res n
    | n == 0 = res
    | otherwise = fiboSg b res (b + res) (n-1)

fiboSzamok n = map (fiboSg 0 1 0) [0..n]

fiboN2 n = (fiboSzamok n) !! n

-- II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.

-- **Megoldott feladatok:**

-- - Határozzuk meg egy szám számjegyeinek összegét:
--   I. módszer:

--   ```haskell
--   szOsszeg :: Int -> Int
--   szOsszeg 0 = 0
--   szOsszeg x = ( x `mod` 10 ) + szOsszeg (x `div` 10)

--   > szOsszeg 123
--   ```

--   II. módszer:

--   ```haskell
--   szOsszeg1 :: Int -> Int -> Int
--   szOsszeg1 0 t = t
--   szOsszeg1 x t = szOsszeg1 (x `div` 10) ( t + x `mod` 10 )

--   > szOsszeg1 123 0
--   ```
