data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

main :: IO ()
main = undefined

breadthFirst :: Tree a -> [a]
breadthFirst t = postprocess queue
  where
    queue = t : walk 1 queue
    postprocess = map extract . filter isNode

walk :: Int -> [Tree a] -> [Tree a]
walk 0 _                = []
walk n (Leaf : q)       = walk (n - 1) q
walk n (Node l _ r : q) = l : r : walk (n + 1) q
walk _ _                = error "walk"

extract :: Tree a -> a
extract (Node _ x _) = x
extract _            = error "extract"

isNode :: Tree a -> Bool
isNode Node{} = True
isNode _      = False
