module UltimasCinquenta where
import Data.Char
    -- questao 1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y | x == y = [y]
                | x > y = []
                | x < y = x:(enumFromTo' (x+1) y)
                
    -- questao 2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | x == z = [x]
                      | x > z = []
                      | otherwise = x:(enumFromThenTo' ((y-x)+x) ((y-x)+y) z)
                      
    -- questao 3
maisMais :: [a] -> [a] -> [a]
maisMais [] l = l
maisMais l [] = l
maisMais (x:xs) l = [x]++(maisMais xs l)

    -- questao 4
exclama :: [a] -> Int -> a
exclama (x:xs) 0 = x
exclama (x:xs) n | n > length (x:xs) = error "posição não presente na lista"
                 | otherwise = exclama xs (n-1)
                 
    -- questao 5

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs)++[x]

    -- questao 6
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n [] = []
myTake n (x:xs) = x:(myTake (n-1) xs)

    -- questao 7
drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' n [] = []
drop' n (x:xs) = drop (n-1) xs

     -- questao 8
zipa :: [a] -> [b] -> [(a,b)]
zipa [] l = []
zipa l [] = []
zipa (x:xs) (y:ys) = (x,y):(zipa xs ys)

-- questao 9
eleme :: Eq a => a -> [a] -> Bool
eleme n [] = False
eleme n (x:xs) | n == x = True
              | otherwise = eleme n xs

-- questao 10
replica :: Int -> a -> [a]
replica 0 n = []
replica x n = n:(replica (x-1) n)

-- questao 11
interspersar :: a -> [a] -> [a]
interspersar n [] = []
interspersar n (x:[]) = [x]
interspersar n (x:xs) = [x,n] ++ (interspersar n xs)

-- questao 12
groupa :: Eq a => [a] -> [[a]]
groupa [] = [[]]
groupa (x:xs) = aux 1 (x:xs)
                where aux n (x:[]) = [(replicate n x)]
                      aux n (x:y:xs) | x == y = aux (n+1) (y:xs)
                                     | otherwise = ((replicate n x):(aux 1 (y:xs))) 

  -- questao 13
concate :: [[a]] -> [a]
concate [[]] = []
concate (x:[]) = x
concate (x:xs) = x++(concate xs)

-- questao 14
initz :: [a] -> [[a]]
initz [] = [[]]
initz (x:[]) = [[],[x]]
initz (x:xs) = (initz xs)++[(x:xs)]

-- questao 15
tailz :: [a] -> [[a]]
tailz [] = [[]]
tailz (x:[]) = [[x],[]]
tailz (x:xs) = (x:xs):(tailz xs)

-- questao 16
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] [] = True
isPrefixOf' l [] = False
isPrefixOf' [] l = True
isPrefixOf' (x:xs) (y:ys) | x == y = isPrefixOf' xs ys
                          | otherwise = False

-- questao 17
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] [] = True
isSuffixOf' l [] = False
isSuffixOf' [] l = False
isSuffixOf' (x:xs) (y:ys) | (length (y:ys)) > (length (x:xs)) = isSuffixOf' (x:xs) (drop ((length (y:ys)) - (length (x:xs))) (y:ys))
                          | x == y = isSuffixOf' xs ys
                          | otherwise = False

-- questao 18
isSubsequencia :: Eq a => [a] -> [a] -> Bool
isSubsequencia [] [] = True
isSubsequencia l [] = False
isSubsequencia [] l = True 
isSubsequencia (x:xs) (y:ys) | x == y = isSubsequencia xs ys
                             | otherwise = isSubsequencia (x:xs) ys
                             
-- questao 19
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices a [] = []
elemIndices a (x:xs) = aux 0 a (x:xs)
                       where aux n a [] = []
                             aux n a (x:xs) | a == x = n:(aux (n+1) a xs)
                                            | otherwise = aux (n+1) a xs
                                            
-- questao 20
nube :: Eq a => [a] -> [a]
nube [] = []
nube (x:xs) | elem x xs = nube xs
            | otherwise = x:(nube xs)
            
-- questao 21
deletez :: Eq a => a -> [a] -> [a]
deletez n [] = []
deletez n (x:xs) | n == x = xs
                 | otherwise = x:(deletez n xs)
                 
-- questao 22
barras :: Eq a => [a] -> [a] -> [a]
barras [] [] = []
barras l [] = l
barras (x:xs) (y:ys) = (barras (aux (x:xs) (y:ys)) ys)
                       where aux [] (y:ys) = []
                             aux (x:xs) (y:ys) | x == y = xs
                                               | otherwise = x:(aux xs (y:ys))
                                               
-- questao 23
uniao :: Eq a => [a] -> [a] -> [a]
uniao [] [] = []
uniao l [] = l
uniao [] l = l
uniao x (y:ys) = (uniao x ys) ++ (aux x y)
                   where aux [] y = [y]
                         aux (x:xs) y | x == y = []
                                      | otherwise = aux xs y
                                      
-- questao 24
interseta :: Eq a => [a] -> [a] -> [a]
interseta [] [] = []
interseta l [] = l
interseta [] l = []
interseta (x:xs) (y:ys) | elem x (y:ys) = x:(interseta xs (y:ys))
                        | otherwise = (interseta xs (y:ys))
                        
-- questao 25
insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) | a > x = x:(insert a xs)
                | otherwise = a:x:xs
                
  -- questao 26
despalavras :: [String] -> String
despalavras [] = ""
despalavras (x:[]) = x
despalavras (x:xs) = x++" "++(despalavras (xs))

-- questao 27
deslinhas :: [String] -> String
deslinhas [] = ""
deslinhas (x:[]) = x
deslinhas (x:xs) = x++"\n"++(deslinhas xs)

-- questao 28
pMaior :: Ord a => [a] -> Int
pMaior l = aux 0 0 l
           where aux n p (x:[]) = n
                 aux n p (x:y:[]) | x >= y = n
                                  | otherwise = p+1
                 aux n p (x:y:xs) | x >= y = aux n (p+1) (x:xs)
                                  | otherwise = aux (n+1) (p+1) (y:xs)
                                  
-- questao 29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (x:xs) | elem x xs = True
                    | otherwise = temRepetidos xs

-- questao 30
algarismos :: [Char] -> [Char] 
algarismos [] = []
algarismos (x:xs) | elem x ['0'..'9'] = x:(algarismos xs)
                  | otherwise = algarismos xs
                  
-- questao 31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares (x:[]) = []
posImpares (x:y:xs) = y:(posImpares xs)

-- questao 32
posPares :: [a] -> [a]
posPares [] = []
posPares (x:[]) = [x]
posPares (x:y:xs) = x:(posPares xs)

-- questao 33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (x:[]) = True
isSorted (x:y:xs) | x <= y = True && isSorted (y:xs)
                  | otherwise = False

-- questao 34
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:[]) = [x]
iSort (x:xs) = insert x (iSort xs)

-- questao 35
menor :: String -> String -> Bool
menor [] [] = False
menor [] l = True
menor l [] = False
menor (x:xs) (y:ys) | (ord x) > (ord y) = False
                    | (ord x) < (ord y) = True
                    | otherwise = True && (menor xs ys)

-- questao 36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet n [] = False
elemMSet n ((a,b):[]) = n == a
elemMSet n ((a,b):xs) | n == a = True
                      | otherwise = elemMSet n xs

-- questao 37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((a,b):[]) = b
lengthMSet ((a,b):xs) = b+(lengthMSet xs)

-- questao 38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):[]) = replicate b a
converteMSet ((a,b):xs) = (replicate b a) ++ (converteMSet xs)

-- questao 39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet n [] = [(n,1)]
insereMSet n ((a,b):xs) | n == a = [(a,(b+1))]
                        | otherwise = (a,b):(insereMSet n xs)

-- questao 40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((b,c):xs) | a == b && (c > 1) = (b,(c-1)):xs
                        | a == b = xs
                        | otherwise = (b,c):(removeMSet a xs)
                        
-- questao 41
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet l = constroi 1 l
                 where constroi n (x:[]) = [(x,n)]
                       constroi n (x:y:xs) | x == y = constroi (n+1) (y:xs)
                                           | otherwise = (x,n):(constroi 1 (y:xs))
                                           
-- questao 42
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers l = (aux l, aux2 l)
                     where aux [] = []
                           aux ((Left a):xs) = a:(aux xs)
                           aux (x:xs) = aux xs
                           aux2 [] = []
                           aux2 ((Right b):xs) = b:(aux2 xs)
                           aux2 (x:xs) = aux2 xs
                           
-- questao 43
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just a):xs) = a:(catMaybes xs)
catMaybes ((Nothing):xs) = catMaybes xs

-- questao 44
data Movimento = Norte | Sul | Este | Oeste
                   deriving Show

posicao :: (Int, Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:xs) = posicao (x,(y+1)) xs
posicao (x,y) (Sul:xs) = posicao (x,(y-1)) xs
posicao (x,y) (Oeste:xs) = posicao ((x-1), y) xs
posicao (x,y) (Este:xs) = posicao ((x+1), y) xs

--questao 45
caminho :: (Int,Int) -> (Int, Int) -> [Movimento]
caminho (x,y) (x1,y1) | x == x1 && y1 > y = Norte:(caminho (x,(y+1)) (x1,y1))
                      | x == x1 && y1 < y = Sul:(caminho (x,(y-1)) (x1,y1))
                      | x == x1 && y1 == y = []
                      | x > x1 = Oeste:(caminho ((x-1),y) (x1,y1))
                      | x < x1 = Este:(caminho ((x+1),y) (x1,y1))

-- questao 46
vertical :: [Movimento] -> Bool
vertical [] = False
vertical (Norte:[]) = True
vertical (Sul:[]) = True
vertical (x:[]) = False
vertical (Norte:xs) = True && vertical xs
vertical (Sul:xs) = True && vertical xs
vertical (x:xs) = False

-- questao 47
data Posicao = Pos Int Int
               deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral ((Pos x y):[]) = Pos x y
maisCentral ((Pos x y):(Pos x1 y1):xs) | sqrt(fromIntegral(y^2 + x^2)) > sqrt(fromIntegral(x1^2 + y1^2)) = maisCentral ((Pos x1 y1):xs)
                                       | otherwise = maisCentral ((Pos x y):xs)         

-- questao 48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x y) ((Pos x1 y1):xs) | (x == x1 || x == (x1+1) || x == (x1-1)) && ((y == y1) || y == (y1-1) || y == (y1+1)) = (Pos x1 y1):(vizinhos (Pos x y) xs)
                                    | otherwise = vizinhos (Pos x y) xs

-- questao 49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada ((Pos x y):[]) = True
mesmaOrdenada ((Pos x y):(Pos x1 y1):xs) | y == y1 = True && mesmaOrdenada ((Pos x1 y1):xs)
                                         | otherwise = False
                                         
                

                                         module UltimasCinquenta where
                                            import Data.Char
                                                -- questao 1
                                            enumFromTo' :: Int -> Int -> [Int]
                                            enumFromTo' x y | x == y = [y]
                                                            | x > y = []
                                                            | x < y = x:(enumFromTo' (x+1) y)
                                                            
                                                -- questao 2
                                            enumFromThenTo' :: Int -> Int -> Int -> [Int]
                                            enumFromThenTo' x y z | x == z = [x]
                                                                  | x > z = []
                                                                  | otherwise = x:(enumFromThenTo' ((y-x)+x) ((y-x)+y) z)
                                                                  
                                                -- questao 3
                                            maisMais :: [a] -> [a] -> [a]
                                            maisMais [] l = l
                                            maisMais l [] = l
                                            maisMais (x:xs) l = [x]++(maisMais xs l)
                                            
                                                -- questao 4
                                            exclama :: [a] -> Int -> a
                                            exclama (x:xs) 0 = x
                                            exclama (x:xs) n | n > length (x:xs) = error "posição não presente na lista"
                                                             | otherwise = exclama xs (n-1)
                                                             
                                                -- questao 5
                                            
                                            myReverse :: [a] -> [a]
                                            myReverse [] = []
                                            myReverse (x:xs) = (myReverse xs)++[x]
                                            
                                                -- questao 6
                                            myTake :: Int -> [a] -> [a]
                                            myTake 0 _ = []
                                            myTake n [] = []
                                            myTake n (x:xs) = x:(myTake (n-1) xs)
                                            
                                                -- questao 7
                                            drop' :: Int -> [a] -> [a]
                                            drop' 0 l = l
                                            drop' n [] = []
                                            drop' n (x:xs) = drop (n-1) xs
                                            
                                                 -- questao 8
                                            zipa :: [a] -> [b] -> [(a,b)]
                                            zipa [] l = []
                                            zipa l [] = []
                                            zipa (x:xs) (y:ys) = (x,y):(zipa xs ys)
                                            
                                            -- questao 9
                                            eleme :: Eq a => a -> [a] -> Bool
                                            eleme n [] = False
                                            eleme n (x:xs) | n == x = True
                                                          | otherwise = eleme n xs
                                            
                                            -- questao 10
                                            replica :: Int -> a -> [a]
                                            replica 0 n = []
                                            replica x n = n:(replica (x-1) n)
                                            
                                            -- questao 11
                                            interspersar :: a -> [a] -> [a]
                                            interspersar n [] = []
                                            interspersar n (x:[]) = [x]
                                            interspersar n (x:xs) = [x,n] ++ (interspersar n xs)
                                            
                                            -- questao 12
                                            groupa :: Eq a => [a] -> [[a]]
                                            groupa [] = [[]]
                                            groupa (x:xs) = aux 1 (x:xs)
                                                            where aux n (x:[]) = [(replicate n x)]
                                                                  aux n (x:y:xs) | x == y = aux (n+1) (y:xs)
                                                                                 | otherwise = ((replicate n x):(aux 1 (y:xs))) 
                                            
                                              -- questao 13
                                            concate :: [[a]] -> [a]
                                            concate [[]] = []
                                            concate (x:[]) = x
                                            concate (x:xs) = x++(concate xs)
                                            
                                            -- questao 14
                                            initz :: [a] -> [[a]]
                                            initz [] = [[]]
                                            initz (x:[]) = [[],[x]]
                                            initz (x:xs) = (initz xs)++[(x:xs)]
                                            
                                            -- questao 15
                                            tailz :: [a] -> [[a]]
                                            tailz [] = [[]]
                                            tailz (x:[]) = [[x],[]]
                                            tailz (x:xs) = (x:xs):(tailz xs)
                                            
                                            -- questao 16
                                            isPrefixOf' :: Eq a => [a] -> [a] -> Bool
                                            isPrefixOf' [] [] = True
                                            isPrefixOf' l [] = False
                                            isPrefixOf' [] l = True
                                            isPrefixOf' (x:xs) (y:ys) | x == y = isPrefixOf' xs ys
                                                                      | otherwise = False
                                            
                                            -- questao 17
                                            isSuffixOf' :: Eq a => [a] -> [a] -> Bool
                                            isSuffixOf' [] [] = True
                                            isSuffixOf' l [] = False
                                            isSuffixOf' [] l = False
                                            isSuffixOf' (x:xs) (y:ys) | (length (y:ys)) > (length (x:xs)) = isSuffixOf' (x:xs) (drop ((length (y:ys)) - (length (x:xs))) (y:ys))
                                                                      | x == y = isSuffixOf' xs ys
                                                                      | otherwise = False
                                            
                                            -- questao 18
                                            isSubsequencia :: Eq a => [a] -> [a] -> Bool
                                            isSubsequencia [] [] = True
                                            isSubsequencia l [] = False
                                            isSubsequencia [] l = True 
                                            isSubsequencia (x:xs) (y:ys) | x == y = isSubsequencia xs ys
                                                                         | otherwise = isSubsequencia (x:xs) ys
                                                                         
                                            -- questao 19
                                            elemIndices :: Eq a => a -> [a] -> [Int]
                                            elemIndices a [] = []
                                            elemIndices a (x:xs) = aux 0 a (x:xs)
                                                                   where aux n a [] = []
                                                                         aux n a (x:xs) | a == x = n:(aux (n+1) a xs)
                                                                                        | otherwise = aux (n+1) a xs
                                                                                        
                                            -- questao 20
                                            nube :: Eq a => [a] -> [a]
                                            nube [] = []
                                            nube (x:xs) | elem x xs = nube xs
                                                        | otherwise = x:(nube xs)
                                                        
                                            -- questao 21
                                            deletez :: Eq a => a -> [a] -> [a]
                                            deletez n [] = []
                                            deletez n (x:xs) | n == x = xs
                                                             | otherwise = x:(deletez n xs)
                                                             
                                            -- questao 22
                                            barras :: Eq a => [a] -> [a] -> [a]
                                            barras [] [] = []
                                            barras l [] = l
                                            barras (x:xs) (y:ys) = (barras (aux (x:xs) (y:ys)) ys)
                                                                   where aux [] (y:ys) = []
                                                                         aux (x:xs) (y:ys) | x == y = xs
                                                                                           | otherwise = x:(aux xs (y:ys))
                                                                                           
                                            -- questao 23
                                            uniao :: Eq a => [a] -> [a] -> [a]
                                            uniao [] [] = []
                                            uniao l [] = l
                                            uniao [] l = l
                                            uniao x (y:ys) = (uniao x ys) ++ (aux x y)
                                                               where aux [] y = [y]
                                                                     aux (x:xs) y | x == y = []
                                                                                  | otherwise = aux xs y
                                                                                  
                                            -- questao 24
                                            interseta :: Eq a => [a] -> [a] -> [a]
                                            interseta [] [] = []
                                            interseta l [] = l
                                            interseta [] l = []
                                            interseta (x:xs) (y:ys) | elem x (y:ys) = x:(interseta xs (y:ys))
                                                                    | otherwise = (interseta xs (y:ys))
                                                                    
                                            -- questao 25
                                            insert :: Ord a => a -> [a] -> [a]
                                            insert a [] = [a]
                                            insert a (x:xs) | a > x = x:(insert a xs)
                                                            | otherwise = a:x:xs
                                                            
                                              -- questao 26
                                            despalavras :: [String] -> String
                                            despalavras [] = ""
                                            despalavras (x:[]) = x
                                            despalavras (x:xs) = x++" "++(despalavras (xs))
                                            
                                            -- questao 27
                                            deslinhas :: [String] -> String
                                            deslinhas [] = ""
                                            deslinhas (x:[]) = x
                                            deslinhas (x:xs) = x++"\n"++(deslinhas xs)
                                            
                                            -- questao 28
                                            pMaior :: Ord a => [a] -> Int
                                            pMaior l = aux 0 0 l
                                                       where aux n p (x:[]) = n
                                                             aux n p (x:y:[]) | x >= y = n
                                                                              | otherwise = p+1
                                                             aux n p (x:y:xs) | x >= y = aux n (p+1) (x:xs)
                                                                              | otherwise = aux (n+1) (p+1) (y:xs)
                                                                              
                                            -- questao 29
                                            temRepetidos :: Eq a => [a] -> Bool
                                            temRepetidos [] = False
                                            temRepetidos (x:xs) | elem x xs = True
                                                                | otherwise = temRepetidos xs
                                            
                                            -- questao 30
                                            algarismos :: [Char] -> [Char] 
                                            algarismos [] = []
                                            algarismos (x:xs) | elem x ['0'..'9'] = x:(algarismos xs)
                                                              | otherwise = algarismos xs
                                                              
                                            -- questao 31
                                            posImpares :: [a] -> [a]
                                            posImpares [] = []
                                            posImpares (x:[]) = []
                                            posImpares (x:y:xs) = y:(posImpares xs)
                                            
                                            -- questao 32
                                            posPares :: [a] -> [a]
                                            posPares [] = []
                                            posPares (x:[]) = [x]
                                            posPares (x:y:xs) = x:(posPares xs)
                                            
                                            -- questao 33
                                            isSorted :: Ord a => [a] -> Bool
                                            isSorted [] = True
                                            isSorted (x:[]) = True
                                            isSorted (x:y:xs) | x <= y = True && isSorted (y:xs)
                                                              | otherwise = False
                                            
                                            -- questao 34
                                            iSort :: Ord a => [a] -> [a]
                                            iSort [] = []
                                            iSort (x:[]) = [x]
                                            iSort (x:xs) = insert x (iSort xs)
                                            
                                            -- questao 35
                                            menor :: String -> String -> Bool
                                            menor [] [] = False
                                            menor [] l = True
                                            menor l [] = False
                                            menor (x:xs) (y:ys) | (ord x) > (ord y) = False
                                                                | (ord x) < (ord y) = True
                                                                | otherwise = True && (menor xs ys)
                                            
                                            -- questao 36
                                            elemMSet :: Eq a => a -> [(a,Int)] -> Bool
                                            elemMSet n [] = False
                                            elemMSet n ((a,b):[]) = n == a
                                            elemMSet n ((a,b):xs) | n == a = True
                                                                  | otherwise = elemMSet n xs
                                            
                                            -- questao 37
                                            lengthMSet :: [(a,Int)] -> Int
                                            lengthMSet [] = 0
                                            lengthMSet ((a,b):[]) = b
                                            lengthMSet ((a,b):xs) = b+(lengthMSet xs)
                                            
                                            -- questao 38
                                            converteMSet :: [(a,Int)] -> [a]
                                            converteMSet [] = []
                                            converteMSet ((a,b):[]) = replicate b a
                                            converteMSet ((a,b):xs) = (replicate b a) ++ (converteMSet xs)
                                            
                                            -- questao 39
                                            insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
                                            insereMSet n [] = [(n,1)]
                                            insereMSet n ((a,b):xs) | n == a = [(a,(b+1))]
                                                                    | otherwise = (a,b):(insereMSet n xs)
                                            
                                            -- questao 40
                                            removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
                                            removeMSet a [] = []
                                            removeMSet a ((b,c):xs) | a == b && (c > 1) = (b,(c-1)):xs
                                                                    | a == b = xs
                                                                    | otherwise = (b,c):(removeMSet a xs)
                                                                    
                                            -- questao 41
                                            constroiMSet :: Ord a => [a] -> [(a,Int)]
                                            constroiMSet [] = []
                                            constroiMSet l = constroi 1 l
                                                             where constroi n (x:[]) = [(x,n)]
                                                                   constroi n (x:y:xs) | x == y = constroi (n+1) (y:xs)
                                                                                       | otherwise = (x,n):(constroi 1 (y:xs))
                                                                                       
                                            -- questao 42
                                            partitionEithers :: [Either a b] -> ([a],[b])
                                            partitionEithers [] = ([],[])
                                            partitionEithers l = (aux l, aux2 l)
                                                                 where aux [] = []
                                                                       aux ((Left a):xs) = a:(aux xs)
                                                                       aux (x:xs) = aux xs
                                                                       aux2 [] = []
                                                                       aux2 ((Right b):xs) = b:(aux2 xs)
                                                                       aux2 (x:xs) = aux2 xs
                                                                       
                                            -- questao 43
                                            catMaybes :: [Maybe a] -> [a]
                                            catMaybes [] = []
                                            catMaybes ((Just a):xs) = a:(catMaybes xs)
                                            catMaybes ((Nothing):xs) = catMaybes xs
                                            
                                            -- questao 44
                                            data Movimento = Norte | Sul | Este | Oeste
                                                               deriving Show
                                            
                                            posicao :: (Int, Int) -> [Movimento] -> (Int,Int)
                                            posicao (x,y) [] = (x,y)
                                            posicao (x,y) (Norte:xs) = posicao (x,(y+1)) xs
                                            posicao (x,y) (Sul:xs) = posicao (x,(y-1)) xs
                                            posicao (x,y) (Oeste:xs) = posicao ((x-1), y) xs
                                            posicao (x,y) (Este:xs) = posicao ((x+1), y) xs
                                            
                                            --questao 45
                                            caminho :: (Int,Int) -> (Int, Int) -> [Movimento]
                                            caminho (x,y) (x1,y1) | x == x1 && y1 > y = Norte:(caminho (x,(y+1)) (x1,y1))
                                                                  | x == x1 && y1 < y = Sul:(caminho (x,(y-1)) (x1,y1))
                                                                  | x == x1 && y1 == y = []
                                                                  | x > x1 = Oeste:(caminho ((x-1),y) (x1,y1))
                                                                  | x < x1 = Este:(caminho ((x+1),y) (x1,y1))
                                            
                                            -- questao 46
                                            vertical :: [Movimento] -> Bool
                                            vertical [] = False
                                            vertical (Norte:[]) = True
                                            vertical (Sul:[]) = True
                                            vertical (x:[]) = False
                                            vertical (Norte:xs) = True && vertical xs
                                            vertical (Sul:xs) = True && vertical xs
                                            vertical (x:xs) = False
                                            
                                            -- questao 47
                                            data Posicao = Pos Int Int
                                                           deriving Show
                                            
                                            maisCentral :: [Posicao] -> Posicao
                                            maisCentral ((Pos x y):[]) = Pos x y
                                            maisCentral ((Pos x y):(Pos x1 y1):xs) | sqrt(fromIntegral(y^2 + x^2)) > sqrt(fromIntegral(x1^2 + y1^2)) = maisCentral ((Pos x1 y1):xs)
                                                                                   | otherwise = maisCentral ((Pos x y):xs)         
                                            
                                            -- questao 48
                                            vizinhos :: Posicao -> [Posicao] -> [Posicao]
                                            vizinhos _ [] = []
                                            vizinhos (Pos x y) ((Pos x1 y1):xs) | (x == x1 || x == (x1+1) || x == (x1-1)) && ((y == y1) || y == (y1-1) || y == (y1+1)) = (Pos x1 y1):(vizinhos (Pos x y) xs)
                                                                                | otherwise = vizinhos (Pos x y) xs
                                            
                                            -- questao 49
                                            mesmaOrdenada :: [Posicao] -> Bool
                                            mesmaOrdenada [] = True
                                            mesmaOrdenada ((Pos x y):[]) = True
                                            mesmaOrdenada ((Pos x y):(Pos x1 y1):xs) | y == y1 = True && mesmaOrdenada ((Pos x1 y1):xs)
                                                                                     | otherwise = False
                                                                                     
                                                            
                                            
                                                                                     module UltimasCinquenta where
                                                                                        import Data.Char
                                                                                            -- questao 1
                                                                                        enumFromTo' :: Int -> Int -> [Int]
                                                                                        enumFromTo' x y | x == y = [y]
                                                                                                        | x > y = []
                                                                                                        | x < y = x:(enumFromTo' (x+1) y)
                                                                                                        
                                                                                            -- questao 2
                                                                                        enumFromThenTo' :: Int -> Int -> Int -> [Int]
                                                                                        enumFromThenTo' x y z | x == z = [x]
                                                                                                              | x > z = []
                                                                                                              | otherwise = x:(enumFromThenTo' ((y-x)+x) ((y-x)+y) z)
                                                                                                              
                                                                                            -- questao 3
                                                                                        maisMais :: [a] -> [a] -> [a]
                                                                                        maisMais [] l = l
                                                                                        maisMais l [] = l
                                                                                        maisMais (x:xs) l = [x]++(maisMais xs l)
                                                                                        
                                                                                            -- questao 4
                                                                                        exclama :: [a] -> Int -> a
                                                                                        exclama (x:xs) 0 = x
                                                                                        exclama (x:xs) n | n > length (x:xs) = error "posição não presente na lista"
                                                                                                         | otherwise = exclama xs (n-1)
                                                                                                         
                                                                                            -- questao 5
                                                                                        
                                                                                        myReverse :: [a] -> [a]
                                                                                        myReverse [] = []
                                                                                        myReverse (x:xs) = (myReverse xs)++[x]
                                                                                        
                                                                                            -- questao 6
                                                                                        myTake :: Int -> [a] -> [a]
                                                                                        myTake 0 _ = []
                                                                                        myTake n [] = []
                                                                                        myTake n (x:xs) = x:(myTake (n-1) xs)
                                                                                        
                                                                                            -- questao 7
                                                                                        drop' :: Int -> [a] -> [a]
                                                                                        drop' 0 l = l
                                                                                        drop' n [] = []
                                                                                        drop' n (x:xs) = drop (n-1) xs
                                                                                        
                                                                                             -- questao 8
                                                                                        zipa :: [a] -> [b] -> [(a,b)]
                                                                                        zipa [] l = []
                                                                                        zipa l [] = []
                                                                                        zipa (x:xs) (y:ys) = (x,y):(zipa xs ys)
                                                                                        
                                                                                        -- questao 9
                                                                                        eleme :: Eq a => a -> [a] -> Bool
                                                                                        eleme n [] = False
                                                                                        eleme n (x:xs) | n == x = True
                                                                                                      | otherwise = eleme n xs
                                                                                        
                                                                                        -- questao 10
                                                                                        replica :: Int -> a -> [a]
                                                                                        replica 0 n = []
                                                                                        replica x n = n:(replica (x-1) n)
                                                                                        
                                                                                        -- questao 11
                                                                                        interspersar :: a -> [a] -> [a]
                                                                                        interspersar n [] = []
                                                                                        interspersar n (x:[]) = [x]
                                                                                        interspersar n (x:xs) = [x,n] ++ (interspersar n xs)
                                                                                        
                                                                                        -- questao 12
                                                                                        groupa :: Eq a => [a] -> [[a]]
                                                                                        groupa [] = [[]]
                                                                                        groupa (x:xs) = aux 1 (x:xs)
                                                                                                        where aux n (x:[]) = [(replicate n x)]
                                                                                                              aux n (x:y:xs) | x == y = aux (n+1) (y:xs)
                                                                                                                             | otherwise = ((replicate n x):(aux 1 (y:xs))) 
                                                                                        
                                                                                          -- questao 13
                                                                                        concate :: [[a]] -> [a]
                                                                                        concate [[]] = []
                                                                                        concate (x:[]) = x
                                                                                        concate (x:xs) = x++(concate xs)
                                                                                        
                                                                                        -- questao 14
                                                                                        initz :: [a] -> [[a]]
                                                                                        initz [] = [[]]
                                                                                        initz (x:[]) = [[],[x]]
                                                                                        initz (x:xs) = (initz xs)++[(x:xs)]
                                                                                        
                                                                                        -- questao 15
                                                                                        tailz :: [a] -> [[a]]
                                                                                        tailz [] = [[]]
                                                                                        tailz (x:[]) = [[x],[]]
                                                                                        tailz (x:xs) = (x:xs):(tailz xs)
                                                                                        
                                                                                        -- questao 16
                                                                                        isPrefixOf' :: Eq a => [a] -> [a] -> Bool
                                                                                        isPrefixOf' [] [] = True
                                                                                        isPrefixOf' l [] = False
                                                                                        isPrefixOf' [] l = True
                                                                                        isPrefixOf' (x:xs) (y:ys) | x == y = isPrefixOf' xs ys
                                                                                                                  | otherwise = False
                                                                                        
                                                                                        -- questao 17
                                                                                        isSuffixOf' :: Eq a => [a] -> [a] -> Bool
                                                                                        isSuffixOf' [] [] = True
                                                                                        isSuffixOf' l [] = False
                                                                                        isSuffixOf' [] l = False
                                                                                        isSuffixOf' (x:xs) (y:ys) | (length (y:ys)) > (length (x:xs)) = isSuffixOf' (x:xs) (drop ((length (y:ys)) - (length (x:xs))) (y:ys))
                                                                                                                  | x == y = isSuffixOf' xs ys
                                                                                                                  | otherwise = False
                                                                                        
                                                                                        -- questao 18
                                                                                        isSubsequencia :: Eq a => [a] -> [a] -> Bool
                                                                                        isSubsequencia [] [] = True
                                                                                        isSubsequencia l [] = False
                                                                                        isSubsequencia [] l = True 
                                                                                        isSubsequencia (x:xs) (y:ys) | x == y = isSubsequencia xs ys
                                                                                                                     | otherwise = isSubsequencia (x:xs) ys
                                                                                                                     
                                                                                        -- questao 19
                                                                                        elemIndices :: Eq a => a -> [a] -> [Int]
                                                                                        elemIndices a [] = []
                                                                                        elemIndices a (x:xs) = aux 0 a (x:xs)
                                                                                                               where aux n a [] = []
                                                                                                                     aux n a (x:xs) | a == x = n:(aux (n+1) a xs)
                                                                                                                                    | otherwise = aux (n+1) a xs
                                                                                                                                    
                                                                                        -- questao 20
                                                                                        nube :: Eq a => [a] -> [a]
                                                                                        nube [] = []
                                                                                        nube (x:xs) | elem x xs = nube xs
                                                                                                    | otherwise = x:(nube xs)
                                                                                                    
                                                                                        -- questao 21
                                                                                        deletez :: Eq a => a -> [a] -> [a]
                                                                                        deletez n [] = []
                                                                                        deletez n (x:xs) | n == x = xs
                                                                                                         | otherwise = x:(deletez n xs)
                                                                                                         
                                                                                        -- questao 22
                                                                                        barras :: Eq a => [a] -> [a] -> [a]
                                                                                        barras [] [] = []
                                                                                        barras l [] = l
                                                                                        barras (x:xs) (y:ys) = (barras (aux (x:xs) (y:ys)) ys)
                                                                                                               where aux [] (y:ys) = []
                                                                                                                     aux (x:xs) (y:ys) | x == y = xs
                                                                                                                                       | otherwise = x:(aux xs (y:ys))
                                                                                                                                       
                                                                                        -- questao 23
                                                                                        uniao :: Eq a => [a] -> [a] -> [a]
                                                                                        uniao [] [] = []
                                                                                        uniao l [] = l
                                                                                        uniao [] l = l
                                                                                        uniao x (y:ys) = (uniao x ys) ++ (aux x y)
                                                                                                           where aux [] y = [y]
                                                                                                                 aux (x:xs) y | x == y = []
                                                                                                                              | otherwise = aux xs y
                                                                                                                              
                                                                                        -- questao 24
                                                                                        interseta :: Eq a => [a] -> [a] -> [a]
                                                                                        interseta [] [] = []
                                                                                        interseta l [] = l
                                                                                        interseta [] l = []
                                                                                        interseta (x:xs) (y:ys) | elem x (y:ys) = x:(interseta xs (y:ys))
                                                                                                                | otherwise = (interseta xs (y:ys))
                                                                                                                
                                                                                        -- questao 25
                                                                                        insert :: Ord a => a -> [a] -> [a]
                                                                                        insert a [] = [a]
                                                                                        insert a (x:xs) | a > x = x:(insert a xs)
                                                                                                        | otherwise = a:x:xs
                                                                                                        
                                                                                          -- questao 26
                                                                                        despalavras :: [String] -> String
                                                                                        despalavras [] = ""
                                                                                        despalavras (x:[]) = x
                                                                                        despalavras (x:xs) = x++" "++(despalavras (xs))
                                                                                        
                                                                                        -- questao 27
                                                                                        deslinhas :: [String] -> String
                                                                                        deslinhas [] = ""
                                                                                        deslinhas (x:[]) = x
                                                                                        deslinhas (x:xs) = x++"\n"++(deslinhas xs)
                                                                                        
                                                                                        -- questao 28
                                                                                        pMaior :: Ord a => [a] -> Int
                                                                                        pMaior l = aux 0 0 l
                                                                                                   where aux n p (x:[]) = n
                                                                                                         aux n p (x:y:[]) | x >= y = n
                                                                                                                          | otherwise = p+1
                                                                                                         aux n p (x:y:xs) | x >= y = aux n (p+1) (x:xs)
                                                                                                                          | otherwise = aux (n+1) (p+1) (y:xs)
                                                                                                                          
                                                                                        -- questao 29
                                                                                        temRepetidos :: Eq a => [a] -> Bool
                                                                                        temRepetidos [] = False
                                                                                        temRepetidos (x:xs) | elem x xs = True
                                                                                                            | otherwise = temRepetidos xs
                                                                                        
                                                                                        -- questao 30
                                                                                        algarismos :: [Char] -> [Char] 
                                                                                        algarismos [] = []
                                                                                        algarismos (x:xs) | elem x ['0'..'9'] = x:(algarismos xs)
                                                                                                          | otherwise = algarismos xs
                                                                                                          
                                                                                        -- questao 31
                                                                                        posImpares :: [a] -> [a]
                                                                                        posImpares [] = []
                                                                                        posImpares (x:[]) = []
                                                                                        posImpares (x:y:xs) = y:(posImpares xs)
                                                                                        
                                                                                        -- questao 32
                                                                                        posPares :: [a] -> [a]
                                                                                        posPares [] = []
                                                                                        posPares (x:[]) = [x]
                                                                                        posPares (x:y:xs) = x:(posPares xs)
                                                                                        
                                                                                        -- questao 33
                                                                                        isSorted :: Ord a => [a] -> Bool
                                                                                        isSorted [] = True
                                                                                        isSorted (x:[]) = True
                                                                                        isSorted (x:y:xs) | x <= y = True && isSorted (y:xs)
                                                                                                          | otherwise = False
                                                                                        
                                                                                        -- questao 34
                                                                                        iSort :: Ord a => [a] -> [a]
                                                                                        iSort [] = []
                                                                                        iSort (x:[]) = [x]
                                                                                        iSort (x:xs) = insert x (iSort xs)
                                                                                        
                                                                                        -- questao 35
                                                                                        menor :: String -> String -> Bool
                                                                                        menor [] [] = False
                                                                                        menor [] l = True
                                                                                        menor l [] = False
                                                                                        menor (x:xs) (y:ys) | (ord x) > (ord y) = False
                                                                                                            | (ord x) < (ord y) = True
                                                                                                            | otherwise = True && (menor xs ys)
                                                                                        
                                                                                        -- questao 36
                                                                                        elemMSet :: Eq a => a -> [(a,Int)] -> Bool
                                                                                        elemMSet n [] = False
                                                                                        elemMSet n ((a,b):[]) = n == a
                                                                                        elemMSet n ((a,b):xs) | n == a = True
                                                                                                              | otherwise = elemMSet n xs
                                                                                        
                                                                                        -- questao 37
                                                                                        lengthMSet :: [(a,Int)] -> Int
                                                                                        lengthMSet [] = 0
                                                                                        lengthMSet ((a,b):[]) = b
                                                                                        lengthMSet ((a,b):xs) = b+(lengthMSet xs)
                                                                                        
                                                                                        -- questao 38
                                                                                        converteMSet :: [(a,Int)] -> [a]
                                                                                        converteMSet [] = []
                                                                                        converteMSet ((a,b):[]) = replicate b a
                                                                                        converteMSet ((a,b):xs) = (replicate b a) ++ (converteMSet xs)
                                                                                        
                                                                                        -- questao 39
                                                                                        insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
                                                                                        insereMSet n [] = [(n,1)]
                                                                                        insereMSet n ((a,b):xs) | n == a = [(a,(b+1))]
                                                                                                                | otherwise = (a,b):(insereMSet n xs)
                                                                                        
                                                                                        -- questao 40
                                                                                        removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
                                                                                        removeMSet a [] = []
                                                                                        removeMSet a ((b,c):xs) | a == b && (c > 1) = (b,(c-1)):xs
                                                                                                                | a == b = xs
                                                                                                                | otherwise = (b,c):(removeMSet a xs)
                                                                                                                
                                                                                        -- questao 41
                                                                                        constroiMSet :: Ord a => [a] -> [(a,Int)]
                                                                                        constroiMSet [] = []
                                                                                        constroiMSet l = constroi 1 l
                                                                                                         where constroi n (x:[]) = [(x,n)]
                                                                                                               constroi n (x:y:xs) | x == y = constroi (n+1) (y:xs)
                                                                                                                                   | otherwise = (x,n):(constroi 1 (y:xs))
                                                                                                                                   
                                                                                        -- questao 42
                                                                                        partitionEithers :: [Either a b] -> ([a],[b])
                                                                                        partitionEithers [] = ([],[])
                                                                                        partitionEithers l = (aux l, aux2 l)
                                                                                                             where aux [] = []
                                                                                                                   aux ((Left a):xs) = a:(aux xs)
                                                                                                                   aux (x:xs) = aux xs
                                                                                                                   aux2 [] = []
                                                                                                                   aux2 ((Right b):xs) = b:(aux2 xs)
                                                                                                                   aux2 (x:xs) = aux2 xs
                                                                                                                   
                                                                                        -- questao 43
                                                                                        catMaybes :: [Maybe a] -> [a]
                                                                                        catMaybes [] = []
                                                                                        catMaybes ((Just a):xs) = a:(catMaybes xs)
                                                                                        catMaybes ((Nothing):xs) = catMaybes xs
                                                                                        
                                                                                        -- questao 44
                                                                                        data Movimento = Norte | Sul | Este | Oeste
                                                                                                           deriving Show
                                                                                        
                                                                                        posicao :: (Int, Int) -> [Movimento] -> (Int,Int)
                                                                                        posicao (x,y) [] = (x,y)
                                                                                        posicao (x,y) (Norte:xs) = posicao (x,(y+1)) xs
                                                                                        posicao (x,y) (Sul:xs) = posicao (x,(y-1)) xs
                                                                                        posicao (x,y) (Oeste:xs) = posicao ((x-1), y) xs
                                                                                        posicao (x,y) (Este:xs) = posicao ((x+1), y) xs
                                                                                        
                                                                                        --questao 45
                                                                                        caminho :: (Int,Int) -> (Int, Int) -> [Movimento]
                                                                                        caminho (x,y) (x1,y1) | x == x1 && y1 > y = Norte:(caminho (x,(y+1)) (x1,y1))
                                                                                                              | x == x1 && y1 < y = Sul:(caminho (x,(y-1)) (x1,y1))
                                                                                                              | x == x1 && y1 == y = []
                                                                                                              | x > x1 = Oeste:(caminho ((x-1),y) (x1,y1))
                                                                                                              | x < x1 = Este:(caminho ((x+1),y) (x1,y1))
                                                                                        
                                                                                        -- questao 46
                                                                                        vertical :: [Movimento] -> Bool
                                                                                        vertical [] = False
                                                                                        vertical (Norte:[]) = True
                                                                                        vertical (Sul:[]) = True
                                                                                        vertical (x:[]) = False
                                                                                        vertical (Norte:xs) = True && vertical xs
                                                                                        vertical (Sul:xs) = True && vertical xs
                                                                                        vertical (x:xs) = False
                                                                                        
                                                                                        -- questao 47
                                                                                        data Posicao = Pos Int Int
                                                                                                       deriving Show
                                                                                        
                                                                                        maisCentral :: [Posicao] -> Posicao
                                                                                        maisCentral ((Pos x y):[]) = Pos x y
                                                                                        maisCentral ((Pos x y):(Pos x1 y1):xs) | sqrt(fromIntegral(y^2 + x^2)) > sqrt(fromIntegral(x1^2 + y1^2)) = maisCentral ((Pos x1 y1):xs)
                                                                                                                               | otherwise = maisCentral ((Pos x y):xs)         
                                                                                        
                                                                                        -- questao 48
                                                                                        vizinhos :: Posicao -> [Posicao] -> [Posicao]
                                                                                        vizinhos _ [] = []
                                                                                        vizinhos (Pos x y) ((Pos x1 y1):xs) | (x == x1 || x == (x1+1) || x == (x1-1)) && ((y == y1) || y == (y1-1) || y == (y1+1)) = (Pos x1 y1):(vizinhos (Pos x y) xs)
                                                                                                                            | otherwise = vizinhos (Pos x y) xs
                                                                                        
                                                                                        -- questao 49
                                                                                        mesmaOrdenada :: [Posicao] -> Bool
                                                                                        mesmaOrdenada [] = True
                                                                                        mesmaOrdenada ((Pos x y):[]) = True
                                                                                        mesmaOrdenada ((Pos x y):(Pos x1 y1):xs) | y == y1 = True && mesmaOrdenada ((Pos x1 y1):xs)
                                                                                                                                 | otherwise = False
                                                                                                                                 
                                                                                                        
                                                                                        
                                                                                                                                 module UltimasCinquenta where
                                                                                                                                    import Data.Char
                                                                                                                                        -- questao 1
                                                                                                                                    enumFromTo' :: Int -> Int -> [Int]
                                                                                                                                    enumFromTo' x y | x == y = [y]
                                                                                                                                                    | x > y = []
                                                                                                                                                    | x < y = x:(enumFromTo' (x+1) y)
                                                                                                                                                    
                                                                                                                                        -- questao 2
                                                                                                                                    enumFromThenTo' :: Int -> Int -> Int -> [Int]
                                                                                                                                    enumFromThenTo' x y z | x == z = [x]
                                                                                                                                                          | x > z = []
                                                                                                                                                          | otherwise = x:(enumFromThenTo' ((y-x)+x) ((y-x)+y) z)
                                                                                                                                                          
                                                                                                                                        -- questao 3
                                                                                                                                    maisMais :: [a] -> [a] -> [a]
                                                                                                                                    maisMais [] l = l
                                                                                                                                    maisMais l [] = l
                                                                                                                                    maisMais (x:xs) l = [x]++(maisMais xs l)
                                                                                                                                    
                                                                                                                                        -- questao 4
                                                                                                                                    exclama :: [a] -> Int -> a
                                                                                                                                    exclama (x:xs) 0 = x
                                                                                                                                    exclama (x:xs) n | n > length (x:xs) = error "posição não presente na lista"
                                                                                                                                                     | otherwise = exclama xs (n-1)
                                                                                                                                                     
                                                                                                                                        -- questao 5
                                                                                                                                    
                                                                                                                                    myReverse :: [a] -> [a]
                                                                                                                                    myReverse [] = []
                                                                                                                                    myReverse (x:xs) = (myReverse xs)++[x]
                                                                                                                                    
                                                                                                                                        -- questao 6
                                                                                                                                    myTake :: Int -> [a] -> [a]
                                                                                                                                    myTake 0 _ = []
                                                                                                                                    myTake n [] = []
                                                                                                                                    myTake n (x:xs) = x:(myTake (n-1) xs)
                                                                                                                                    
                                                                                                                                        -- questao 7
                                                                                                                                    drop' :: Int -> [a] -> [a]
                                                                                                                                    drop' 0 l = l
                                                                                                                                    drop' n [] = []
                                                                                                                                    drop' n (x:xs) = drop (n-1) xs
                                                                                                                                    
                                                                                                                                         -- questao 8
                                                                                                                                    zipa :: [a] -> [b] -> [(a,b)]
                                                                                                                                    zipa [] l = []
                                                                                                                                    zipa l [] = []
                                                                                                                                    zipa (x:xs) (y:ys) = (x,y):(zipa xs ys)
                                                                                                                                    
                                                                                                                                    -- questao 9
                                                                                                                                    eleme :: Eq a => a -> [a] -> Bool
                                                                                                                                    eleme n [] = False
                                                                                                                                    eleme n (x:xs) | n == x = True
                                                                                                                                                  | otherwise = eleme n xs
                                                                                                                                    
                                                                                                                                    -- questao 10
                                                                                                                                    replica :: Int -> a -> [a]
                                                                                                                                    replica 0 n = []
                                                                                                                                    replica x n = n:(replica (x-1) n)
                                                                                                                                    
                                                                                                                                    -- questao 11
                                                                                                                                    interspersar :: a -> [a] -> [a]
                                                                                                                                    interspersar n [] = []
                                                                                                                                    interspersar n (x:[]) = [x]
                                                                                                                                    interspersar n (x:xs) = [x,n] ++ (interspersar n xs)
                                                                                                                                    
                                                                                                                                    -- questao 12
                                                                                                                                    groupa :: Eq a => [a] -> [[a]]
                                                                                                                                    groupa [] = [[]]
                                                                                                                                    groupa (x:xs) = aux 1 (x:xs)
                                                                                                                                                    where aux n (x:[]) = [(replicate n x)]
                                                                                                                                                          aux n (x:y:xs) | x == y = aux (n+1) (y:xs)
                                                                                                                                                                         | otherwise = ((replicate n x):(aux 1 (y:xs))) 
                                                                                                                                    
                                                                                                                                      -- questao 13
                                                                                                                                    concate :: [[a]] -> [a]
                                                                                                                                    concate [[]] = []
                                                                                                                                    concate (x:[]) = x
                                                                                                                                    concate (x:xs) = x++(concate xs)
                                                                                                                                    
                                                                                                                                    -- questao 14
                                                                                                                                    initz :: [a] -> [[a]]
                                                                                                                                    initz [] = [[]]
                                                                                                                                    initz (x:[]) = [[],[x]]
                                                                                                                                    initz (x:xs) = (initz xs)++[(x:xs)]
                                                                                                                                    
                                                                                                                                    -- questao 15
                                                                                                                                    tailz :: [a] -> [[a]]
                                                                                                                                    tailz [] = [[]]
                                                                                                                                    tailz (x:[]) = [[x],[]]
                                                                                                                                    tailz (x:xs) = (x:xs):(tailz xs)
                                                                                                                                    
                                                                                                                                    -- questao 16
                                                                                                                                    isPrefixOf' :: Eq a => [a] -> [a] -> Bool
                                                                                                                                    isPrefixOf' [] [] = True
                                                                                                                                    isPrefixOf' l [] = False
                                                                                                                                    isPrefixOf' [] l = True
                                                                                                                                    isPrefixOf' (x:xs) (y:ys) | x == y = isPrefixOf' xs ys
                                                                                                                                                              | otherwise = False
                                                                                                                                    
                                                                                                                                    -- questao 17
                                                                                                                                    isSuffixOf' :: Eq a => [a] -> [a] -> Bool
                                                                                                                                    isSuffixOf' [] [] = True
                                                                                                                                    isSuffixOf' l [] = False
                                                                                                                                    isSuffixOf' [] l = False
                                                                                                                                    isSuffixOf' (x:xs) (y:ys) | (length (y:ys)) > (length (x:xs)) = isSuffixOf' (x:xs) (drop ((length (y:ys)) - (length (x:xs))) (y:ys))
                                                                                                                                                              | x == y = isSuffixOf' xs ys
                                                                                                                                                              | otherwise = False
                                                                                                                                    
                                                                                                                                    -- questao 18
                                                                                                                                    isSubsequencia :: Eq a => [a] -> [a] -> Bool
                                                                                                                                    isSubsequencia [] [] = True
                                                                                                                                    isSubsequencia l [] = False
                                                                                                                                    isSubsequencia [] l = True 
                                                                                                                                    isSubsequencia (x:xs) (y:ys) | x == y = isSubsequencia xs ys
                                                                                                                                                                 | otherwise = isSubsequencia (x:xs) ys
                                                                                                                                                                 
                                                                                                                                    -- questao 19
                                                                                                                                    elemIndices :: Eq a => a -> [a] -> [Int]
                                                                                                                                    elemIndices a [] = []
                                                                                                                                    elemIndices a (x:xs) = aux 0 a (x:xs)
                                                                                                                                                           where aux n a [] = []
                                                                                                                                                                 aux n a (x:xs) | a == x = n:(aux (n+1) a xs)
                                                                                                                                                                                | otherwise = aux (n+1) a xs
                                                                                                                                                                                
                                                                                                                                    -- questao 20
                                                                                                                                    nube :: Eq a => [a] -> [a]
                                                                                                                                    nube [] = []
                                                                                                                                    nube (x:xs) | elem x xs = nube xs
                                                                                                                                                | otherwise = x:(nube xs)
                                                                                                                                                
                                                                                                                                    -- questao 21
                                                                                                                                    deletez :: Eq a => a -> [a] -> [a]
                                                                                                                                    deletez n [] = []
                                                                                                                                    deletez n (x:xs) | n == x = xs
                                                                                                                                                     | otherwise = x:(deletez n xs)
                                                                                                                                                     
                                                                                                                                    -- questao 22
                                                                                                                                    barras :: Eq a => [a] -> [a] -> [a]
                                                                                                                                    barras [] [] = []
                                                                                                                                    barras l [] = l
                                                                                                                                    barras (x:xs) (y:ys) = (barras (aux (x:xs) (y:ys)) ys)
                                                                                                                                                           where aux [] (y:ys) = []
                                                                                                                                                                 aux (x:xs) (y:ys) | x == y = xs
                                                                                                                                                                                   | otherwise = x:(aux xs (y:ys))
                                                                                                                                                                                   
                                                                                                                                    -- questao 23
                                                                                                                                    uniao :: Eq a => [a] -> [a] -> [a]
                                                                                                                                    uniao [] [] = []
                                                                                                                                    uniao l [] = l
                                                                                                                                    uniao [] l = l
                                                                                                                                    uniao x (y:ys) = (uniao x ys) ++ (aux x y)
                                                                                                                                                       where aux [] y = [y]
                                                                                                                                                             aux (x:xs) y | x == y = []
                                                                                                                                                                          | otherwise = aux xs y
                                                                                                                                                                          
                                                                                                                                    -- questao 24
                                                                                                                                    interseta :: Eq a => [a] -> [a] -> [a]
                                                                                                                                    interseta [] [] = []
                                                                                                                                    interseta l [] = l
                                                                                                                                    interseta [] l = []
                                                                                                                                    interseta (x:xs) (y:ys) | elem x (y:ys) = x:(interseta xs (y:ys))
                                                                                                                                                            | otherwise = (interseta xs (y:ys))
                                                                                                                                                            
                                                                                                                                    -- questao 25
                                                                                                                                    insert :: Ord a => a -> [a] -> [a]
                                                                                                                                    insert a [] = [a]
                                                                                                                                    insert a (x:xs) | a > x = x:(insert a xs)
                                                                                                                                                    | otherwise = a:x:xs
                                                                                                                                                    
                                                                                                                                      -- questao 26
                                                                                                                                    despalavras :: [String] -> String
                                                                                                                                    despalavras [] = ""
                                                                                                                                    despalavras (x:[]) = x
                                                                                                                                    despalavras (x:xs) = x++" "++(despalavras (xs))
                                                                                                                                    
                                                                                                                                    -- questao 27
                                                                                                                                    deslinhas :: [String] -> String
                                                                                                                                    deslinhas [] = ""
                                                                                                                                    deslinhas (x:[]) = x
                                                                                                                                    deslinhas (x:xs) = x++"\n"++(deslinhas xs)
                                                                                                                                    
                                                                                                                                    -- questao 28
                                                                                                                                    pMaior :: Ord a => [a] -> Int
                                                                                                                                    pMaior l = aux 0 0 l
                                                                                                                                               where aux n p (x:[]) = n
                                                                                                                                                     aux n p (x:y:[]) | x >= y = n
                                                                                                                                                                      | otherwise = p+1
                                                                                                                                                     aux n p (x:y:xs) | x >= y = aux n (p+1) (x:xs)
                                                                                                                                                                      | otherwise = aux (n+1) (p+1) (y:xs)
                                                                                                                                                                      
                                                                                                                                    -- questao 29
                                                                                                                                    temRepetidos :: Eq a => [a] -> Bool
                                                                                                                                    temRepetidos [] = False
                                                                                                                                    temRepetidos (x:xs) | elem x xs = True
                                                                                                                                                        | otherwise = temRepetidos xs
                                                                                                                                    
                                                                                                                                    -- questao 30
                                                                                                                                    algarismos :: [Char] -> [Char] 
                                                                                                                                    algarismos [] = []
                                                                                                                                    algarismos (x:xs) | elem x ['0'..'9'] = x:(algarismos xs)
                                                                                                                                                      | otherwise = algarismos xs
                                                                                                                                                      
                                                                                                                                    -- questao 31
                                                                                                                                    posImpares :: [a] -> [a]
                                                                                                                                    posImpares [] = []
                                                                                                                                    posImpares (x:[]) = []
                                                                                                                                    posImpares (x:y:xs) = y:(posImpares xs)
                                                                                                                                    
                                                                                                                                    -- questao 32
                                                                                                                                    posPares :: [a] -> [a]
                                                                                                                                    posPares [] = []
                                                                                                                                    posPares (x:[]) = [x]
                                                                                                                                    posPares (x:y:xs) = x:(posPares xs)
                                                                                                                                    
                                                                                                                                    -- questao 33
                                                                                                                                    isSorted :: Ord a => [a] -> Bool
                                                                                                                                    isSorted [] = True
                                                                                                                                    isSorted (x:[]) = True
                                                                                                                                    isSorted (x:y:xs) | x <= y = True && isSorted (y:xs)
                                                                                                                                                      | otherwise = False
                                                                                                                                    
                                                                                                                                    -- questao 34
                                                                                                                                    iSort :: Ord a => [a] -> [a]
                                                                                                                                    iSort [] = []
                                                                                                                                    iSort (x:[]) = [x]
                                                                                                                                    iSort (x:xs) = insert x (iSort xs)
                                                                                                                                    
                                                                                                                                    -- questao 35
                                                                                                                                    menor :: String -> String -> Bool
                                                                                                                                    menor [] [] = False
                                                                                                                                    menor [] l = True
                                                                                                                                    menor l [] = False
                                                                                                                                    menor (x:xs) (y:ys) | (ord x) > (ord y) = False
                                                                                                                                                        | (ord x) < (ord y) = True
                                                                                                                                                        | otherwise = True && (menor xs ys)
                                                                                                                                    
                                                                                                                                    -- questao 36
                                                                                                                                    elemMSet :: Eq a => a -> [(a,Int)] -> Bool
                                                                                                                                    elemMSet n [] = False
                                                                                                                                    elemMSet n ((a,b):[]) = n == a
                                                                                                                                    elemMSet n ((a,b):xs) | n == a = True
                                                                                                                                                          | otherwise = elemMSet n xs
                                                                                                                                    
                                                                                                                                    -- questao 37
                                                                                                                                    lengthMSet :: [(a,Int)] -> Int
                                                                                                                                    lengthMSet [] = 0
                                                                                                                                    lengthMSet ((a,b):[]) = b
                                                                                                                                    lengthMSet ((a,b):xs) = b+(lengthMSet xs)
                                                                                                                                    
                                                                                                                                    -- questao 38
                                                                                                                                    converteMSet :: [(a,Int)] -> [a]
                                                                                                                                    converteMSet [] = []
                                                                                                                                    converteMSet ((a,b):[]) = replicate b a
                                                                                                                                    converteMSet ((a,b):xs) = (replicate b a) ++ (converteMSet xs)
                                                                                                                                    
                                                                                                                                    -- questao 39
                                                                                                                                    insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
                                                                                                                                    insereMSet n [] = [(n,1)]
                                                                                                                                    insereMSet n ((a,b):xs) | n == a = [(a,(b+1))]
                                                                                                                                                            | otherwise = (a,b):(insereMSet n xs)
                                                                                                                                    
                                                                                                                                    -- questao 40
                                                                                                                                    removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
                                                                                                                                    removeMSet a [] = []
                                                                                                                                    removeMSet a ((b,c):xs) | a == b && (c > 1) = (b,(c-1)):xs
                                                                                                                                                            | a == b = xs
                                                                                                                                                            | otherwise = (b,c):(removeMSet a xs)
                                                                                                                                                            
                                                                                                                                    -- questao 41
                                                                                                                                    constroiMSet :: Ord a => [a] -> [(a,Int)]
                                                                                                                                    constroiMSet [] = []
                                                                                                                                    constroiMSet l = constroi 1 l
                                                                                                                                                     where constroi n (x:[]) = [(x,n)]
                                                                                                                                                           constroi n (x:y:xs) | x == y = constroi (n+1) (y:xs)
                                                                                                                                                                               | otherwise = (x,n):(constroi 1 (y:xs))
                                                                                                                                                                               
                                                                                                                                    -- questao 42
                                                                                                                                    partitionEithers :: [Either a b] -> ([a],[b])
                                                                                                                                    partitionEithers [] = ([],[])
                                                                                                                                    partitionEithers l = (aux l, aux2 l)
                                                                                                                                                         where aux [] = []
                                                                                                                                                               aux ((Left a):xs) = a:(aux xs)
                                                                                                                                                               aux (x:xs) = aux xs
                                                                                                                                                               aux2 [] = []
                                                                                                                                                               aux2 ((Right b):xs) = b:(aux2 xs)
                                                                                                                                                               aux2 (x:xs) = aux2 xs
                                                                                                                                                               
                                                                                                                                    -- questao 43
                                                                                                                                    catMaybes :: [Maybe a] -> [a]
                                                                                                                                    catMaybes [] = []
                                                                                                                                    catMaybes ((Just a):xs) = a:(catMaybes xs)
                                                                                                                                    catMaybes ((Nothing):xs) = catMaybes xs
                                                                                                                                    
                                                                                                                                    -- questao 44
                                                                                                                                    data Movimento = Norte | Sul | Este | Oeste
                                                                                                                                                       deriving Show
                                                                                                                                    
                                                                                                                                    posicao :: (Int, Int) -> [Movimento] -> (Int,Int)
                                                                                                                                    posicao (x,y) [] = (x,y)
                                                                                                                                    posicao (x,y) (Norte:xs) = posicao (x,(y+1)) xs
                                                                                                                                    posicao (x,y) (Sul:xs) = posicao (x,(y-1)) xs
                                                                                                                                    posicao (x,y) (Oeste:xs) = posicao ((x-1), y) xs
                                                                                                                                    posicao (x,y) (Este:xs) = posicao ((x+1), y) xs
                                                                                                                                    
                                                                                                                                    --questao 45
                                                                                                                                    caminho :: (Int,Int) -> (Int, Int) -> [Movimento]
                                                                                                                                    caminho (x,y) (x1,y1) | x == x1 && y1 > y = Norte:(caminho (x,(y+1)) (x1,y1))
                                                                                                                                                          | x == x1 && y1 < y = Sul:(caminho (x,(y-1)) (x1,y1))
                                                                                                                                                          | x == x1 && y1 == y = []
                                                                                                                                                          | x > x1 = Oeste:(caminho ((x-1),y) (x1,y1))
                                                                                                                                                          | x < x1 = Este:(caminho ((x+1),y) (x1,y1))
                                                                                                                                    
                                                                                                                                    -- questao 46
                                                                                                                                    vertical :: [Movimento] -> Bool
                                                                                                                                    vertical [] = False
                                                                                                                                    vertical (Norte:[]) = True
                                                                                                                                    vertical (Sul:[]) = True
                                                                                                                                    vertical (x:[]) = False
                                                                                                                                    vertical (Norte:xs) = True && vertical xs
                                                                                                                                    vertical (Sul:xs) = True && vertical xs
                                                                                                                                    vertical (x:xs) = False
                                                                                                                                    
                                                                                                                                    -- questao 47
                                                                                                                                    data Posicao = Pos Int Int
                                                                                                                                                   deriving Show
                                                                                                                                    
                                                                                                                                    maisCentral :: [Posicao] -> Posicao
                                                                                                                                    maisCentral ((Pos x y):[]) = Pos x y
                                                                                                                                    maisCentral ((Pos x y):(Pos x1 y1):xs) | sqrt(fromIntegral(y^2 + x^2)) > sqrt(fromIntegral(x1^2 + y1^2)) = maisCentral ((Pos x1 y1):xs)
                                                                                                                                                                           | otherwise = maisCentral ((Pos x y):xs)         
                                                                                                                                    
                                                                                                                                    -- questao 48
                                                                                                                                    vizinhos :: Posicao -> [Posicao] -> [Posicao]
                                                                                                                                    vizinhos _ [] = []
                                                                                                                                    vizinhos (Pos x y) ((Pos x1 y1):xs) | (x == x1 || x == (x1+1) || x == (x1-1)) && ((y == y1) || y == (y1-1) || y == (y1+1)) = (Pos x1 y1):(vizinhos (Pos x y) xs)
                                                                                                                                                                        | otherwise = vizinhos (Pos x y) xs
                                                                                                                                    
                                                                                                                                    -- questao 49
                                                                                                                                    mesmaOrdenada :: [Posicao] -> Bool
                                                                                                                                    mesmaOrdenada [] = True
                                                                                                                                    mesmaOrdenada ((Pos x y):[]) = True
                                                                                                                                    mesmaOrdenada ((Pos x y):(Pos x1 y1):xs) | y == y1 = True && mesmaOrdenada ((Pos x1 y1):xs)
                                                                                                                                                                             | otherwise = False
                                                                                                                                                                             
                                                                                                                                                    
                                                                                                                                    
                                                                                                                                                                             module UltimasCinquenta where
                                                                                                                                                                                import Data.Char
                                                                                                                                                                                    -- questao 1
                                                                                                                                                                                enumFromTo' :: Int -> Int -> [Int]
                                                                                                                                                                                enumFromTo' x y | x == y = [y]
                                                                                                                                                                                                | x > y = []
                                                                                                                                                                                                | x < y = x:(enumFromTo' (x+1) y)
                                                                                                                                                                                                
                                                                                                                                                                                    -- questao 2
                                                                                                                                                                                enumFromThenTo' :: Int -> Int -> Int -> [Int]
                                                                                                                                                                                enumFromThenTo' x y z | x == z = [x]
                                                                                                                                                                                                      | x > z = []
                                                                                                                                                                                                      | otherwise = x:(enumFromThenTo' ((y-x)+x) ((y-x)+y) z)
                                                                                                                                                                                                      
                                                                                                                                                                                    -- questao 3
                                                                                                                                                                                maisMais :: [a] -> [a] -> [a]
                                                                                                                                                                                maisMais [] l = l
                                                                                                                                                                                maisMais l [] = l
                                                                                                                                                                                maisMais (x:xs) l = [x]++(maisMais xs l)
                                                                                                                                                                                
                                                                                                                                                                                    -- questao 4
                                                                                                                                                                                exclama :: [a] -> Int -> a
                                                                                                                                                                                exclama (x:xs) 0 = x
                                                                                                                                                                                exclama (x:xs) n | n > length (x:xs) = error "posição não presente na lista"
                                                                                                                                                                                                 | otherwise = exclama xs (n-1)
                                                                                                                                                                                                 
                                                                                                                                                                                    -- questao 5
                                                                                                                                                                                
                                                                                                                                                                                myReverse :: [a] -> [a]
                                                                                                                                                                                myReverse [] = []
                                                                                                                                                                                myReverse (x:xs) = (myReverse xs)++[x]
                                                                                                                                                                                
                                                                                                                                                                                    -- questao 6
                                                                                                                                                                                myTake :: Int -> [a] -> [a]
                                                                                                                                                                                myTake 0 _ = []
                                                                                                                                                                                myTake n [] = []
                                                                                                                                                                                myTake n (x:xs) = x:(myTake (n-1) xs)
                                                                                                                                                                                
                                                                                                                                                                                    -- questao 7
                                                                                                                                                                                drop' :: Int -> [a] -> [a]
                                                                                                                                                                                drop' 0 l = l
                                                                                                                                                                                drop' n [] = []
                                                                                                                                                                                drop' n (x:xs) = drop (n-1) xs
                                                                                                                                                                                
                                                                                                                                                                                     -- questao 8
                                                                                                                                                                                zipa :: [a] -> [b] -> [(a,b)]
                                                                                                                                                                                zipa [] l = []
                                                                                                                                                                                zipa l [] = []
                                                                                                                                                                                zipa (x:xs) (y:ys) = (x,y):(zipa xs ys)
                                                                                                                                                                                
                                                                                                                                                                                -- questao 9
                                                                                                                                                                                eleme :: Eq a => a -> [a] -> Bool
                                                                                                                                                                                eleme n [] = False
                                                                                                                                                                                eleme n (x:xs) | n == x = True
                                                                                                                                                                                              | otherwise = eleme n xs
                                                                                                                                                                                
                                                                                                                                                                                -- questao 10
                                                                                                                                                                                replica :: Int -> a -> [a]
                                                                                                                                                                                replica 0 n = []
                                                                                                                                                                                replica x n = n:(replica (x-1) n)
                                                                                                                                                                                
                                                                                                                                                                                -- questao 11
                                                                                                                                                                                interspersar :: a -> [a] -> [a]
                                                                                                                                                                                interspersar n [] = []
                                                                                                                                                                                interspersar n (x:[]) = [x]
                                                                                                                                                                                interspersar n (x:xs) = [x,n] ++ (interspersar n xs)
                                                                                                                                                                                
                                                                                                                                                                                -- questao 12
                                                                                                                                                                                groupa :: Eq a => [a] -> [[a]]
                                                                                                                                                                                groupa [] = [[]]
                                                                                                                                                                                groupa (x:xs) = aux 1 (x:xs)
                                                                                                                                                                                                where aux n (x:[]) = [(replicate n x)]
                                                                                                                                                                                                      aux n (x:y:xs) | x == y = aux (n+1) (y:xs)
                                                                                                                                                                                                                     | otherwise = ((replicate n x):(aux 1 (y:xs))) 
                                                                                                                                                                                
                                                                                                                                                                                  -- questao 13
                                                                                                                                                                                concate :: [[a]] -> [a]
                                                                                                                                                                                concate [[]] = []
                                                                                                                                                                                concate (x:[]) = x
                                                                                                                                                                                concate (x:xs) = x++(concate xs)
                                                                                                                                                                                
                                                                                                                                                                                -- questao 14
                                                                                                                                                                                initz :: [a] -> [[a]]
                                                                                                                                                                                initz [] = [[]]
                                                                                                                                                                                initz (x:[]) = [[],[x]]
                                                                                                                                                                                initz (x:xs) = (initz xs)++[(x:xs)]
                                                                                                                                                                                
                                                                                                                                                                                -- questao 15
                                                                                                                                                                                tailz :: [a] -> [[a]]
                                                                                                                                                                                tailz [] = [[]]
                                                                                                                                                                                tailz (x:[]) = [[x],[]]
                                                                                                                                                                                tailz (x:xs) = (x:xs):(tailz xs)
                                                                                                                                                                                
                                                                                                                                                                                -- questao 16
                                                                                                                                                                                isPrefixOf' :: Eq a => [a] -> [a] -> Bool
                                                                                                                                                                                isPrefixOf' [] [] = True
                                                                                                                                                                                isPrefixOf' l [] = False
                                                                                                                                                                                isPrefixOf' [] l = True
                                                                                                                                                                                isPrefixOf' (x:xs) (y:ys) | x == y = isPrefixOf' xs ys
                                                                                                                                                                                                          | otherwise = False
                                                                                                                                                                                
                                                                                                                                                                                -- questao 17
                                                                                                                                                                                isSuffixOf' :: Eq a => [a] -> [a] -> Bool
                                                                                                                                                                                isSuffixOf' [] [] = True
                                                                                                                                                                                isSuffixOf' l [] = False
                                                                                                                                                                                isSuffixOf' [] l = False
                                                                                                                                                                                isSuffixOf' (x:xs) (y:ys) | (length (y:ys)) > (length (x:xs)) = isSuffixOf' (x:xs) (drop ((length (y:ys)) - (length (x:xs))) (y:ys))
                                                                                                                                                                                                          | x == y = isSuffixOf' xs ys
                                                                                                                                                                                                          | otherwise = False
                                                                                                                                                                                
                                                                                                                                                                                -- questao 18
                                                                                                                                                                                isSubsequencia :: Eq a => [a] -> [a] -> Bool
                                                                                                                                                                                isSubsequencia [] [] = True
                                                                                                                                                                                isSubsequencia l [] = False
                                                                                                                                                                                isSubsequencia [] l = True 
                                                                                                                                                                                isSubsequencia (x:xs) (y:ys) | x == y = isSubsequencia xs ys
                                                                                                                                                                                                             | otherwise = isSubsequencia (x:xs) ys
                                                                                                                                                                                                             
                                                                                                                                                                                -- questao 19
                                                                                                                                                                                elemIndices :: Eq a => a -> [a] -> [Int]
                                                                                                                                                                                elemIndices a [] = []
                                                                                                                                                                                elemIndices a (x:xs) = aux 0 a (x:xs)
                                                                                                                                                                                                       where aux n a [] = []
                                                                                                                                                                                                             aux n a (x:xs) | a == x = n:(aux (n+1) a xs)
                                                                                                                                                                                                                            | otherwise = aux (n+1) a xs
                                                                                                                                                                                                                            
                                                                                                                                                                                -- questao 20
                                                                                                                                                                                nube :: Eq a => [a] -> [a]
                                                                                                                                                                                nube [] = []
                                                                                                                                                                                nube (x:xs) | elem x xs = nube xs
                                                                                                                                                                                            | otherwise = x:(nube xs)
                                                                                                                                                                                            
                                                                                                                                                                                -- questao 21
                                                                                                                                                                                deletez :: Eq a => a -> [a] -> [a]
                                                                                                                                                                                deletez n [] = []
                                                                                                                                                                                deletez n (x:xs) | n == x = xs
                                                                                                                                                                                                 | otherwise = x:(deletez n xs)
                                                                                                                                                                                                 
                                                                                                                                                                                -- questao 22
                                                                                                                                                                                barras :: Eq a => [a] -> [a] -> [a]
                                                                                                                                                                                barras [] [] = []
                                                                                                                                                                                barras l [] = l
                                                                                                                                                                                barras (x:xs) (y:ys) = (barras (aux (x:xs) (y:ys)) ys)
                                                                                                                                                                                                       where aux [] (y:ys) = []
                                                                                                                                                                                                             aux (x:xs) (y:ys) | x == y = xs
                                                                                                                                                                                                                               | otherwise = x:(aux xs (y:ys))
                                                                                                                                                                                                                               
                                                                                                                                                                                -- questao 23
                                                                                                                                                                                uniao :: Eq a => [a] -> [a] -> [a]
                                                                                                                                                                                uniao [] [] = []
                                                                                                                                                                                uniao l [] = l
                                                                                                                                                                                uniao [] l = l
                                                                                                                                                                                uniao x (y:ys) = (uniao x ys) ++ (aux x y)
                                                                                                                                                                                                   where aux [] y = [y]
                                                                                                                                                                                                         aux (x:xs) y | x == y = []
                                                                                                                                                                                                                      | otherwise = aux xs y
                                                                                                                                                                                                                      
                                                                                                                                                                                -- questao 24
                                                                                                                                                                                interseta :: Eq a => [a] -> [a] -> [a]
                                                                                                                                                                                interseta [] [] = []
                                                                                                                                                                                interseta l [] = l
                                                                                                                                                                                interseta [] l = []
                                                                                                                                                                                interseta (x:xs) (y:ys) | elem x (y:ys) = x:(interseta xs (y:ys))
                                                                                                                                                                                                        | otherwise = (interseta xs (y:ys))
                                                                                                                                                                                                        
                                                                                                                                                                                -- questao 25
                                                                                                                                                                                insert :: Ord a => a -> [a] -> [a]
                                                                                                                                                                                insert a [] = [a]
                                                                                                                                                                                insert a (x:xs) | a > x = x:(insert a xs)
                                                                                                                                                                                                | otherwise = a:x:xs
                                                                                                                                                                                                
                                                                                                                                                                                  -- questao 26
                                                                                                                                                                                despalavras :: [String] -> String
                                                                                                                                                                                despalavras [] = ""
                                                                                                                                                                                despalavras (x:[]) = x
                                                                                                                                                                                despalavras (x:xs) = x++" "++(despalavras (xs))
                                                                                                                                                                                
                                                                                                                                                                                -- questao 27
                                                                                                                                                                                deslinhas :: [String] -> String
                                                                                                                                                                                deslinhas [] = ""
                                                                                                                                                                                deslinhas (x:[]) = x
                                                                                                                                                                                deslinhas (x:xs) = x++"\n"++(deslinhas xs)
                                                                                                                                                                                
                                                                                                                                                                                -- questao 28
                                                                                                                                                                                pMaior :: Ord a => [a] -> Int
                                                                                                                                                                                pMaior l = aux 0 0 l
                                                                                                                                                                                           where aux n p (x:[]) = n
                                                                                                                                                                                                 aux n p (x:y:[]) | x >= y = n
                                                                                                                                                                                                                  | otherwise = p+1
                                                                                                                                                                                                 aux n p (x:y:xs) | x >= y = aux n (p+1) (x:xs)
                                                                                                                                                                                                                  | otherwise = aux (n+1) (p+1) (y:xs)
                                                                                                                                                                                                                  
                                                                                                                                                                                -- questao 29
                                                                                                                                                                                temRepetidos :: Eq a => [a] -> Bool
                                                                                                                                                                                temRepetidos [] = False
                                                                                                                                                                                temRepetidos (x:xs) | elem x xs = True
                                                                                                                                                                                                    | otherwise = temRepetidos xs
                                                                                                                                                                                
                                                                                                                                                                                -- questao 30
                                                                                                                                                                                algarismos :: [Char] -> [Char] 
                                                                                                                                                                                algarismos [] = []
                                                                                                                                                                                algarismos (x:xs) | elem x ['0'..'9'] = x:(algarismos xs)
                                                                                                                                                                                                  | otherwise = algarismos xs
                                                                                                                                                                                                  
                                                                                                                                                                                -- questao 31
                                                                                                                                                                                posImpares :: [a] -> [a]
                                                                                                                                                                                posImpares [] = []
                                                                                                                                                                                posImpares (x:[]) = []
                                                                                                                                                                                posImpares (x:y:xs) = y:(posImpares xs)
                                                                                                                                                                                
                                                                                                                                                                                -- questao 32
                                                                                                                                                                                posPares :: [a] -> [a]
                                                                                                                                                                                posPares [] = []
                                                                                                                                                                                posPares (x:[]) = [x]
                                                                                                                                                                                posPares (x:y:xs) = x:(posPares xs)
                                                                                                                                                                                
                                                                                                                                                                                -- questao 33
                                                                                                                                                                                isSorted :: Ord a => [a] -> Bool
                                                                                                                                                                                isSorted [] = True
                                                                                                                                                                                isSorted (x:[]) = True
                                                                                                                                                                                isSorted (x:y:xs) | x <= y = True && isSorted (y:xs)
                                                                                                                                                                                                  | otherwise = False
                                                                                                                                                                                
                                                                                                                                                                                -- questao 34
                                                                                                                                                                                iSort :: Ord a => [a] -> [a]
                                                                                                                                                                                iSort [] = []
                                                                                                                                                                                iSort (x:[]) = [x]
                                                                                                                                                                                iSort (x:xs) = insert x (iSort xs)
                                                                                                                                                                                
                                                                                                                                                                                -- questao 35
                                                                                                                                                                                menor :: String -> String -> Bool
                                                                                                                                                                                menor [] [] = False
                                                                                                                                                                                menor [] l = True
                                                                                                                                                                                menor l [] = False
                                                                                                                                                                                menor (x:xs) (y:ys) | (ord x) > (ord y) = False
                                                                                                                                                                                                    | (ord x) < (ord y) = True
                                                                                                                                                                                                    | otherwise = True && (menor xs ys)
                                                                                                                                                                                
                                                                                                                                                                                -- questao 36
                                                                                                                                                                                elemMSet :: Eq a => a -> [(a,Int)] -> Bool
                                                                                                                                                                                elemMSet n [] = False
                                                                                                                                                                                elemMSet n ((a,b):[]) = n == a
                                                                                                                                                                                elemMSet n ((a,b):xs) | n == a = True
                                                                                                                                                                                                      | otherwise = elemMSet n xs
                                                                                                                                                                                
                                                                                                                                                                                -- questao 37
                                                                                                                                                                                lengthMSet :: [(a,Int)] -> Int
                                                                                                                                                                                lengthMSet [] = 0
                                                                                                                                                                                lengthMSet ((a,b):[]) = b
                                                                                                                                                                                lengthMSet ((a,b):xs) = b+(lengthMSet xs)
                                                                                                                                                                                
                                                                                                                                                                                -- questao 38
                                                                                                                                                                                converteMSet :: [(a,Int)] -> [a]
                                                                                                                                                                                converteMSet [] = []
                                                                                                                                                                                converteMSet ((a,b):[]) = replicate b a
                                                                                                                                                                                converteMSet ((a,b):xs) = (replicate b a) ++ (converteMSet xs)
                                                                                                                                                                                
                                                                                                                                                                                -- questao 39
                                                                                                                                                                                insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
                                                                                                                                                                                insereMSet n [] = [(n,1)]
                                                                                                                                                                                insereMSet n ((a,b):xs) | n == a = [(a,(b+1))]
                                                                                                                                                                                                        | otherwise = (a,b):(insereMSet n xs)
                                                                                                                                                                                
                                                                                                                                                                                -- questao 40
                                                                                                                                                                                removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
                                                                                                                                                                                removeMSet a [] = []
                                                                                                                                                                                removeMSet a ((b,c):xs) | a == b && (c > 1) = (b,(c-1)):xs
                                                                                                                                                                                                        | a == b = xs
                                                                                                                                                                                                        | otherwise = (b,c):(removeMSet a xs)
                                                                                                                                                                                                        
                                                                                                                                                                                -- questao 41
                                                                                                                                                                                constroiMSet :: Ord a => [a] -> [(a,Int)]
                                                                                                                                                                                constroiMSet [] = []
                                                                                                                                                                                constroiMSet l = constroi 1 l
                                                                                                                                                                                                 where constroi n (x:[]) = [(x,n)]
                                                                                                                                                                                                       constroi n (x:y:xs) | x == y = constroi (n+1) (y:xs)
                                                                                                                                                                                                                           | otherwise = (x,n):(constroi 1 (y:xs))
                                                                                                                                                                                                                           
                                                                                                                                                                                -- questao 42
                                                                                                                                                                                partitionEithers :: [Either a b] -> ([a],[b])
                                                                                                                                                                                partitionEithers [] = ([],[])
                                                                                                                                                                                partitionEithers l = (aux l, aux2 l)
                                                                                                                                                                                                     where aux [] = []
                                                                                                                                                                                                           aux ((Left a):xs) = a:(aux xs)
                                                                                                                                                                                                           aux (x:xs) = aux xs
                                                                                                                                                                                                           aux2 [] = []
                                                                                                                                                                                                           aux2 ((Right b):xs) = b:(aux2 xs)
                                                                                                                                                                                                           aux2 (x:xs) = aux2 xs
                                                                                                                                                                                                           
                                                                                                                                                                                -- questao 43
                                                                                                                                                                                catMaybes :: [Maybe a] -> [a]
                                                                                                                                                                                catMaybes [] = []
                                                                                                                                                                                catMaybes ((Just a):xs) = a:(catMaybes xs)
                                                                                                                                                                                catMaybes ((Nothing):xs) = catMaybes xs
                                                                                                                                                                                
                                                                                                                                                                                -- questao 44
                                                                                                                                                                                data Movimento = Norte | Sul | Este | Oeste
                                                                                                                                                                                                   deriving Show
                                                                                                                                                                                
                                                                                                                                                                                posicao :: (Int, Int) -> [Movimento] -> (Int,Int)
                                                                                                                                                                                posicao (x,y) [] = (x,y)
                                                                                                                                                                                posicao (x,y) (Norte:xs) = posicao (x,(y+1)) xs
                                                                                                                                                                                posicao (x,y) (Sul:xs) = posicao (x,(y-1)) xs
                                                                                                                                                                                posicao (x,y) (Oeste:xs) = posicao ((x-1), y) xs
                                                                                                                                                                                posicao (x,y) (Este:xs) = posicao ((x+1), y) xs
                                                                                                                                                                                
                                                                                                                                                                                --questao 45
                                                                                                                                                                                caminho :: (Int,Int) -> (Int, Int) -> [Movimento]
                                                                                                                                                                                caminho (x,y) (x1,y1) | x == x1 && y1 > y = Norte:(caminho (x,(y+1)) (x1,y1))
                                                                                                                                                                                                      | x == x1 && y1 < y = Sul:(caminho (x,(y-1)) (x1,y1))
                                                                                                                                                                                                      | x == x1 && y1 == y = []
                                                                                                                                                                                                      | x > x1 = Oeste:(caminho ((x-1),y) (x1,y1))
                                                                                                                                                                                                      | x < x1 = Este:(caminho ((x+1),y) (x1,y1))
                                                                                                                                                                                
                                                                                                                                                                                -- questao 46
                                                                                                                                                                                vertical :: [Movimento] -> Bool
                                                                                                                                                                                vertical [] = False
                                                                                                                                                                                vertical (Norte:[]) = True
                                                                                                                                                                                vertical (Sul:[]) = True
                                                                                                                                                                                vertical (x:[]) = False
                                                                                                                                                                                vertical (Norte:xs) = True && vertical xs
                                                                                                                                                                                vertical (Sul:xs) = True && vertical xs
                                                                                                                                                                                vertical (x:xs) = False
                                                                                                                                                                                
                                                                                                                                                                                -- questao 47
                                                                                                                                                                                data Posicao = Pos Int Int
                                                                                                                                                                                               deriving Show
                                                                                                                                                                                
                                                                                                                                                                                maisCentral :: [Posicao] -> Posicao
                                                                                                                                                                                maisCentral ((Pos x y):[]) = Pos x y
                                                                                                                                                                                maisCentral ((Pos x y):(Pos x1 y1):xs) | sqrt(fromIntegral(y^2 + x^2)) > sqrt(fromIntegral(x1^2 + y1^2)) = maisCentral ((Pos x1 y1):xs)
                                                                                                                                                                                                                       | otherwise = maisCentral ((Pos x y):xs)         
                                                                                                                                                                                
                                                                                                                                                                                -- questao 48
                                                                                                                                                                                vizinhos :: Posicao -> [Posicao] -> [Posicao]
                                                                                                                                                                                vizinhos _ [] = []
                                                                                                                                                                                vizinhos (Pos x y) ((Pos x1 y1):xs) | (x == x1 || x == (x1+1) || x == (x1-1)) && ((y == y1) || y == (y1-1) || y == (y1+1)) = (Pos x1 y1):(vizinhos (Pos x y) xs)
                                                                                                                                                                                                                    | otherwise = vizinhos (Pos x y) xs
                                                                                                                                                                                
                                                                                                                                                                                -- questao 49
                                                                                                                                                                                mesmaOrdenada :: [Posicao] -> Bool
                                                                                                                                                                                mesmaOrdenada [] = True
                                                                                                                                                                                mesmaOrdenada ((Pos x y):[]) = True
                                                                                                                                                                                mesmaOrdenada ((Pos x y):(Pos x1 y1):xs) | y == y1 = True && mesmaOrdenada ((Pos x1 y1):xs)
                                                                                                                                                                                                                         | otherwise = False
                                                                                                                                                                                                                         
                                                                                                                                                                                                
                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                                                                                                                                        