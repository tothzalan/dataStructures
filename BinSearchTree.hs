module BinSearchTree where

data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

createNode :: a -> Tree a
createNode a = Node a Nil Nil

addTree :: Ord a => Tree a -> Tree a -> Tree a
addTree r@(Node rv rl rr) n@(Node nv nr nl)
  | rv > nv = Node rv n rr
  | rv < nv = Node rv rl n

addItem :: Ord a => a -> Tree a -> Tree a
addItem a root@(Node v l r)
  | v == a = error "v == a"
  | v > a = recL a root root
  | v < a = recR a root root
  where
    recL a acc curr@(Node v l r)
      | l == Nil  = addTree acc (createNode a)
      | otherwise = addTree acc (addItem a l)
    recR a acc curr@(Node v l r)
      | r == Nil  = addTree acc (createNode a)
      | otherwise = addTree acc (addItem a r)

search :: Ord a => a -> Tree a -> Maybe a
search k Nil =  Nothing
search k curr@(Node v l r)
  | k == v    = Just k
  | k < v     = search k l
  | otherwise = search k r
