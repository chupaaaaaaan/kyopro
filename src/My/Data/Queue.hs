module My.Data.Queue where

import qualified Data.List as L

-- | 実時間キューの実装（各キュー操作の最悪計算量がすべてO(1)）
data Queue a = Q [a] [a] [a]

emptyQ :: Queue a
emptyQ = Q [] [] []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q f _ _) = null f

pushQ :: Queue a -> a -> Queue a
pushQ (Q f r a) x = execQ (Q f (x:r) a)

popQ :: Queue a -> (a, Queue a)
popQ (Q [] _ _) = error "pop from empty Queue."
popQ (Q (x:f) r a) = (x, execQ (Q f r a))

execQ :: Queue a -> Queue a
execQ (Q f r (_:xs)) = Q f r xs
execQ (Q f r []) = let f' = rotateQ f r []
                   in Q f' [] f'
    where rotateQ :: [a] -> [a] -> [a] -> [a]
          rotateQ [] (y:_) as = y:as
          rotateQ (x:xs) (y:ys) as = x : rotateQ xs ys (y:as)
          rotateQ _ [] _ = error "impossible situation"

-- >>> queue = fromListQ [1,2,3,4,5]
-- >>> addListQ queue [6,7,8,9]
-- 1 :@ 2 :@ 3 :@ 4 :@ 5 :@ 6 :@ 7 :@ 8 :@ 9 :@ EmptyQ
addListQ :: Queue a -> [a] -> Queue a
addListQ = L.foldl' pushQ

fromListQ :: [a] -> Queue a
fromListQ = addListQ emptyQ

singletonQ :: a -> Queue a
singletonQ x = fromListQ [x]

data ViewQ a = EmptyQ | a :@ Queue a

viewQ :: Queue a -> ViewQ a
viewQ q = if isEmptyQ q then EmptyQ
          else let (v, rest) = popQ q
               in v :@ rest

instance Show a => Show (Queue a) where
    show :: Show a => Queue a -> String
    show q = case viewQ q of
        EmptyQ -> "EmptyQ"
        v :@ rest -> show v <> " :@ " <> show rest

