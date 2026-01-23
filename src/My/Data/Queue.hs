module My.Data.Queue where

import qualified Data.List as L

-- | 実時間キューの実装（各キュー操作の最悪計算量がすべてO(1)）
data Queue a = Q !Int ![a] ![a] ![a]

emptyQ :: Queue a
emptyQ = Q 0 [] [] []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q _ f _ _) = null f

sizeQ :: Queue a -> Int
sizeQ (Q n _ _ _) = n

{-# INLINE pushQ #-}
pushQ :: a -> Queue a -> Queue a
pushQ x (Q n f r a) = execQ (Q (n+1) f (x:r) a)

{-# INLINE (.>) #-}
(.>) :: Queue a -> a -> Queue a
q .> x = pushQ x q
infixl 5 .>

{-# INLINE popQ #-}
popQ :: Queue a -> (a, Queue a)
popQ (Q _ [] _ _) = error "pop from empty Queue."
popQ (Q n (x:f) r a) = (x, execQ (Q (n-1) f r a))

{-# INLINE unconsQ #-}
unconsQ :: Queue a -> Maybe (a, Queue a)
unconsQ (Q _ [] _ _) = Nothing
unconsQ (Q n (x:f) r a) = Just (x, execQ (Q (n-1) f r a))

{-# INLINE execQ #-}
execQ :: Queue a -> Queue a
execQ (Q n f r a) = case a of
    (_:xs) -> Q n f r xs
    [] ->let f' = rotateQ f r []
         in Q n f' [] f'
    where rotateQ :: [a] -> [a] -> [a] -> [a]
          rotateQ [] (y:_) as = y:as
          rotateQ (x:xs) (y:ys) as = x : rotateQ xs ys (y:as)
          rotateQ _ [] _ = error "impossible situation"

-- >>> queue = fromListQ [1,2,3,4,5]
-- >>> addListQ queue [6,7,8,9]
-- Q [1,2,3,4,5,6,7,8,9]
addListQ :: Queue a -> [a] -> Queue a
addListQ = L.foldl' (.>)

fromListQ :: [a] -> Queue a
fromListQ = addListQ emptyQ

-- >>> toListQ $ fromListQ [1,2,3,4,5]
-- [1,2,3,4,5]
toListQ :: Queue a -> [a]
toListQ = L.unfoldr unconsQ

singletonQ :: a -> Queue a
singletonQ x = fromListQ [x]

data ViewQ a = EmptyQ | a :@ !(Queue a)

viewQ :: Queue a -> ViewQ a
viewQ q = case unconsQ q of
    Nothing -> EmptyQ
    Just (v, rest) -> v :@ rest

instance Show a => Show (Queue a) where
    show :: Show a => Queue a -> String
    show q = "Q " <> show (toListQ q)
