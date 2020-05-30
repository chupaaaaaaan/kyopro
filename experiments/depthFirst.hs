data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show


main :: IO ()
main = undefined


preorder' :: Tree a -> [a]
preorder' Leaf         = []
preorder' (Node l x r) = [x] ++ preorder' l ++ preorder' r

inorder' :: Tree a -> [a]
inorder' Leaf         = []
inorder' (Node l x r) = inorder' l ++ [x]  ++ inorder' r

postorder' :: Tree a -> [a]
postorder' Leaf         = []
postorder' (Node l x r) = preorder' l ++ preorder' r ++ [x]

preorder :: Tree a -> [a]
preorder t = go t []
  where go Leaf xs         = xs
        go (Node l x r) xs = x : go l (go r xs)

inorder :: Tree a -> [a]
inorder t = go t []
  where go Leaf xs         = xs
        go (Node l x r) xs = go l (x : go r xs)

postorder :: Tree a -> [a]
postorder t = go t []
  where go Leaf xs         = xs
        go (Node l x r) xs = go l (go r (x:xs))
