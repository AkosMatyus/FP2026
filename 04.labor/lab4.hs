-- # 4. labor

-- I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

-- - az első n páros szám négyzetét,
parosNegyzet n = take n [i^2 | i<-[2,4..]]

parosNegyzet2 n = [i ^ 2 | i<-[2,4..(n*2)]]

-- - az első $$[1, 2, 2, 3, 3, 3, 4, 4, 4, 4,\ldots]$$,
szamokLs n
    |n/=1=szamokLs(n - 1)++replicate n n
    |otherwise = replicate n n

-- - az első $$[2, 4, 4, 6, 6, 6, 8, 8, 8, 8\ldots]$$,
szamokLs3 n i
    | i /= n = replicate i (i*2) ++ szamokLs3 n (i + 1)
    | otherwise = replicate i (i*2)

-- - az első $$[n, n-1, \ldots, 2, 1, 1, 2, \ldots, n-1, n]$$,
szamokLs4 n = [n,n-1 .. 1] ++ [1..n]

-- - váltakozva tartalmazzon True és False értékeket,
tfLS n = take n ls
    where 
        ls =[True,False] ++ ls


-- - váltakozva tartalmazza a $$0,\ 1,\ -1$$ értékeket.

szamok n = take n ls
    where 
        ls =[-1,0,1] ++ ls


-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy adott szám osztóinak számát,
osztok n =[i | i<-[1..n], mod n i==0]
osztokSzama n = length $ osztok n

-- - meghatározza egy adott szám legnagyobb páratlan osztóját,
maxParatlanOszto n = last $ filter odd $ osztok n

maxParatlanOszto2 n = last [i | i<-[1,3..n],mod n i ==0]

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
decP x p
    | x<p = [x]
    | otherwise = decP(div x p) p ++ [mod x p]

decPMax x p = maximum $ decP x p


-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,
-- - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.

fibo = fiboSg 0 1 0
    where
        fiboSg a b res =res : fiboSg b res (res + b)

fiboAB a b = dropWhile (<a) . takeWhile (<b) $ fibo

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista pozitív elemeinek átlagát,
atlag ls = sum ls / fromIntegral(length ls)
pozAtlag ls = atlag [i | i<-ls,i>0]

-- - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,
listaN ls n = [i | (idx,i) <- zip [1 ..] ls , mod idx n==0]

-- - tükrözi egy lista elemeit,


-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
-- - meghatározza egy lista leggyakrabban előforduló elemét.
