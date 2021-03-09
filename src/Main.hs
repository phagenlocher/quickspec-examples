module Main where

import QuickSpec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Poly(OrdA(..))
import Data.List hiding ( insert )
import Data.Ord ( comparing )
import Data.Complex

{- Simple laws about addition -}
addSig :: [Sig]
addSig = [
  con "+" ((+) :: Int -> Int -> Int),
  con "0" (0 :: Int)
  ]

{- Simple laws about concat and map -}
concatMapSig :: [Sig]
concatMapSig = [
  con "map" (map :: (A -> B) -> [A] -> [B]),
  con "concat" (concat :: [[A]] -> [A]),
  con "concatMap" (concatMap :: (A -> [B]) -> [A] -> [B]),
  con "[]" ([] :: [A]),
  con ":" ((:) :: A -> [A] -> [A])
  ]

{- Laws about equivalence of two functions -}
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

revAcc :: [a] -> [a]
revAcc = aux []
  where
    aux acc [] = acc
    aux acc (x:xs) = aux (x:acc) xs

revSig :: [Sig]
revSig = [
  con "rev" (rev :: [A] -> [A]),
  con "revAcc" (revAcc :: [A] -> [A]),
  con "[]" ([] :: [A])
  ]

{- Laws about sortedness -}
sorted :: Ord a => [a] -> Bool 
sorted [] = True 
sorted [x] = True 
sorted (x:y:xs) = x <= y && sorted (y:xs)

sortedSig :: [Sig]
sortedSig = [
  predicate "sorted" (sorted :: [Int] -> Bool),
  background [
    predicate "<" ((<) :: Int -> Int -> Bool),
    prelude
    ]
  ]

{- Many, many laws about foldr -}
foldrSig :: [Sig]
foldrSig = [
  con "foldr" (foldr :: (A -> B -> B) -> B -> [A] -> B),
  background [
    con "length" (length :: [A] -> Int),
    con "const" (const :: A -> B -> A),
    con "+" ((+) :: Int -> Int -> Int),
    con "1" (1 :: Int),
    con "0" (0 :: Int)
    ]
  ]

{- Custom datatypes similar to https://github.com/nick8325/quickspec/blob/master/examples/Heaps.hs -}
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Typeable

instance Eq a => Eq (Tree a) where
  t1 == t2 = inorder t1 == inorder t2

instance Ord a => Ord (Tree a) where
  compare = comparing inorder

instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = fromList <$> arbitrary

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node l v r) = inorder l ++ [v] ++ inorder r

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

insert :: Ord a => a -> Tree a -> Tree a 
insert v Leaf = Node Leaf v Leaf
insert v (Node l vt r)
  | v <= vt = Node (insert v l) vt r
  | otherwise = Node l vt (insert v r)

mirror :: Tree a -> Tree a
mirror Leaf = Leaf
mirror (Node l v r) = Node (mirror r) v (mirror l)

{- Some laws on trees -}
treeSig1 :: [Sig]
treeSig1 = [
  monoType (Proxy :: Proxy OrdA),
  monoTypeWithVars ["t", "t1", "t2"] (Proxy :: Proxy (Tree OrdA)),
  monoTypeWithVars ["t", "t1", "t2"] (Proxy :: Proxy (Tree Int)),
  con "mirror" (mirror :: Tree OrdA -> Tree OrdA),
  con "rev" (rev :: [OrdA] -> [OrdA]),
  con "fromList" (fromList :: [OrdA] -> Tree OrdA),
  con "insert" (insert :: OrdA -> Tree OrdA -> Tree OrdA)
  ]

{- A real challange... -}
treeSig2 :: [Sig]
treeSig2 = [
  monoTypeWithVars ["a", "b", "c"] (Proxy :: Proxy OrdA),
  monoTypeWithVars ["t", "t1", "t2"] (Proxy :: Proxy (Tree OrdA)),
  monoTypeWithVars ["t", "t1", "t2"] (Proxy :: Proxy (Tree Int)),
  con "fromList" (fromList :: [OrdA] -> Tree OrdA),
  con "inorder" (inorder :: Tree OrdA -> [OrdA]),
  predicate "sorted" (sorted :: [OrdA] -> Bool)
  ]

{- A "complex" example... observational equivalence -}
complexSig :: [Sig]
complexSig = [
  monoTypeObserve (Proxy :: Proxy Float),
  --monoType (Proxy :: Proxy (Complex Int)),
  monoTypeObserve (Proxy :: Proxy (Complex Float)),
  con "realPart" (realPart :: Complex Float -> Float),
  con "imagPart" (imagPart :: Complex Float -> Float),
  con ":+" ((:+) :: Float -> Float -> Complex Float),
  background [ prelude ]
  ]

main :: IO ()
main = return ()