module My.Data.Queue where

data Queue a = Q { front :: [a]
                 , rear  :: [a]
                 , thunk :: [a]
                 } deriving (Eq)

isEmpty :: Eq a => Queue a -> Bool
isEmpty = (==empty)

empty :: Queue a
empty = Q [] [] []

pushBack :: Queue a -> a -> Queue a
pushBack (Q f r t) e = check $! Q f (e:r) t

popFront :: Queue a -> (a, Queue a)
popFront (Q (e:f) r t) = (e, check $! Q f r t)
popFront (Q [] _ _)    = error "empty queue"

check :: Queue a -> Queue a
check (Q f r (_:t)) = Q f r t
check (Q f r []) = let ff = rotate f r []
                   in Q ff [] ff
  where rotate :: [a] -> [a] -> [a] -> [a]
        rotate [] (y:_) zs      = y:zs
        rotate (x:xs) (y:ys) zs = x : rotate xs ys (y:zs)

instance Show a => Show (Queue a) where
  show (Q f r t) = "Q { " <> show f <> "\n" <>
                   "    " <> show r <> "\n" <>
                   "    " <> show t <> "\n" <>
                   "  }"
