{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

newtype DiffList a = DiffList { runDiffList :: [a] -> [a]}

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

instance Semigroup (DiffList a) => Monoid (DiffList a) where
  mempty = DiffList (\xs -> ([] ++ xs))


toDiffList :: [a] -> DiffList a
toDiffList zs = DiffList (\xs -> zs ++ xs)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []
