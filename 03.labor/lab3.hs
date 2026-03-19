import Control.Monad.Trans.Cont (reset)
import System.Win32 (COORD(xPos), pAGE_EXECUTE)
-- # 3. labor

-- I. Mit csinálnak az alábbi függvényhívások, ahol az atlag a számok átlagát meghatározó függvény?

-- ```haskell
-- atlag :: (Floating a) => [a] -> a
-- atlag ls = (sum ls) / fromIntegral (length ls)

-- > (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > (take 4 . reverse . filter odd ) [1..20]
-- > take 4 . reverse . filter odd $ [1..20]
-- > take 4 ( reverse ( filter odd [1..20]))
-- > take 4 $ reverse $ filter odd $ [1..20]
-- ```

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista elemszámát, 2 módszerrel (myLength),
myLength [] = 0
myLength (x : xs) = 1 + myLength xs


myLength2 [] res  = res
myLength2 (x : xs) res = myLength2 xs (res + 1)

myLength3 ls = foldr (\x -> (+) 1) 0 ls

myLength4 ls = foldl (\db x -> db + 1) 0 ls

myLength5 ls res = foldr (\x res -> 1 + res) res ls

-- - összeszorozza a lista elemeit, 2 módszerrel (myProduct),
myProduct [] = 1
myProduct ls = x * myProduct xs
    where
        (x : xs) = ls

myProduct2 [] res = res
myProduct2 (x : xs) res = myProduct2 xs (res * x)


myProduct3 ls = foldr (\x -> (*) x) 1 ls

myProduct4 ls res = foldr (\x res -> (*) x res) res ls

myProduct5 ls = foldr1 (\x -> (*) x) ls

-- - meghatározza egy lista legkisebb elemét (myMinimum),
myMinimum [x] = x
myMinimum (x1:x2:xs) = if x1 < x2 then myMinimum (x1 : xs) else myMinimum (x2 : xs)

myMinimum2 [x] = x
myMinimum2 (x1 : x2 : xs)
    | x1 < x2 = myMinimum2 (x1 : xs)
    | otherwise = myMinimum2 (x2 : xs)

myMinimum3 ls = foldr1 min ls

myMinimum4 ls = minimum ls

-- - meghatározza egy lista legnagyobb elemét (myMaximum),
myMaximum [x] = x
myMaximum (x1 : x2 : xs) = if x1 > x2 then myMaximum (x1:xs) else myMaximum (x2:xs)

myMaximum2 ls = foldr1 (\x1 x2 -> if x1 > x2 then x1 else x2) ls

myMaximum3 ls = foldr1 max ls

myMaximum4 ls = maximum ls

-- - meghatározza egy lista n-ik elemét (!!),
listaN ls n = ls !! n

listaN2 ls n
    | null ls = error "ures lista"
    | n < 0 = error "neg. index"
    | length ls <= n = error " tul nagy index"
    | otherwise = ls !! n


-- - egymásután fűzi a paraméterként megadott két listát (++),
listaFuz ls1 ls2 = ls1 ++ ls2

-- - megállapítja egy listáról, hogy az palindrom-e vagy sem,
palindrom ls = if ls == reverse ls then "palindrom" else "nem palindrom"

palindrom2 ls
    | ls == reverse ls = "palindrom"
    | otherwise = "nem palindrom"

palindrom3 [] = True
palindrom3 [x] = True
palindrom3 ls = if head ls == last ls then palindrom3 ((init . tail) ls) else False

-- - meghatározza egy egész szám számjegyeinek listáját,
szjLs x
    | x < 0 = szjLs (abs x)
    | x < 10 = [x]
    | otherwise = szjLs (div x 10) ++ [mod x 10]


-- - a lista első elemét elköltözteti a lista végére,
elsoUtolso [] = error "ures lista"
elsoUtolso (x : xs) = xs ++ [x]

elsoUtolso2 ls = tail ls ++ [head ls]

-- - meghatározza egy egész elemű lista elemeinek átlagértékét,
-- - meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját,
decP x p 
    | x < p = [x]
    | otherwise = decP (div x p) p ++ [mod x p]

-- - meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot.
pDec ls p = foldl (\hatvany x -> x + (p*hatvany)) ls 

pDec2 x p =
    let 
        szamjegyek x 
            | x < 10 = [x]
            | otherwise = mod x 10 : szamjegyek (div x 10)
        szjIndex = zip (szamjegyek x) [0..]
    in sum [szam * (p ^ hatvany) | (szam, hatvany) <- szjIndex]

-- III. Alkalmazzuk a map függvényt a II.-nél megírt függvényekre.

-- IV. Írjunk egy Haskell függvényt, amely meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét.
aLs = [3, -2, 5, -7]
x0 = 2
poli [] x = 0
poli (a : aLs) x = a + x * (poli aLs x)

-- V. Ha adva van egy P pont koordinátája a kétdimenziós síkban, és adott az lsP pontok egy listája, írjunk egy Haskell függvényt, amely meghatározza azt az lsP-beli P1 pontot, amely legközelebb van a P ponthoz.
type Pont = (Double, Double)

lsP :: [Pont]
lsP = [(4.5,6.2), (1.2, 3.4), (6,8), (4.5,2.4)]

p :: Pont = (3.4, 5.6)
tavolsag (x1, y1) (x2, y2) = sqrt ((x1-x2)**2 + (y1-y2) ** 2)

minTavolsag lsP p = foldl1 (\p1 p2 -> if tavolsag p1 p < tavolsag p2 p then p1 else p2) lsP

minTavolsag2 lsP p = foldl1 aux lsP
    where
        aux p1 p2 = if tavolsag p1 p < tavolsag p2 p then p1 else p2

minTavolsag3 [p1] p = p1 
minTavolsag3 (p1 : p2 : ls) p
    | tavolsag p1 p < tavolsag p2 p = minTavolsag3 (p1 : ls) p
    | otherwise = minTavolsag3 (p2 : ls) p