{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
module My.Data.Rational where

import qualified Data.Vector.Unboxing as VU
import Data.Ratio

-- | 有理数のTupleによる表示
-- Ratio IntがUnboxed Vectorに載らないことへの対応
-- 利便性のため、分母が0の場合も許容する
type Numerator = Int
type Denominator = Int
newtype Rat = Rat { unRat :: (Denominator, Numerator) }
    deriving Eq deriving newtype VU.Unboxable

instance Show Rat where
    show (Rat (x,y)) = show y <> "%" <> show x

instance Num Rat where
    Rat (x1,y1) + Rat (x2,y2) = (x1*x2) |% (y1*x2+y2*x1)
    Rat (x1,y1) - Rat (x2,y2) = (x1*x2) |% (y1*x2-y2*x1)
    Rat (x1,y1) * Rat (x2,y2) = (x1*x2) |% (y1*y2)
    fromInteger n = Rat (1,fromInteger n)
    abs (Rat (x,y)) = Rat (x, abs y)
    signum (Rat (_,y)) = Rat (1,signum y)

instance Ord Rat where
    Rat (x1,y1) <= Rat (x2,y2) = case (x1,x2) of
        (0,0) -> error "Can't compare rational value"
        (0,_) -> y1 < 0
        (_,0) -> y2 >= 0
        _anot -> y1*x2 <= y2*x1

instance Fractional Rat where
    recip (Rat (x1,y1)) = y1 |% x1
    fromRational x = fromIntegral (denominator x) |% fromIntegral (numerator x)

-- | 有理数のコンストラクタ
-- >>> 4 |% 3
-- R(4,3)
--
-- >>> 6 |% 4
-- R(3,2)
--
-- >>> 4 |% 0
-- R(1,0)
--
-- >>> 0 |% 4
-- R(0,1)
(|%) ::Denominator -> Numerator -> Rat
(|%) 0 0 = error "Undefined rational value: (0,0)"
(|%) 0 _ = Rat (0,1)
(|%) _ 0 = Rat (1,0)
(|%) den num = Rat (abs den`div`d, (num * signum den)`div`d)
    where d = gcd num den

infixl 7 |%

-- | 分母を取得する
denom :: Rat -> Denominator
denom (Rat (den,_)) = den

-- | 分子を取得する
numer :: Rat -> Numerator
numer (Rat (_,num)) = num

-- | 有理数をDouble型の値に変換する
rat2Double :: Rat -> Double
rat2Double (Rat (den,num))
    | den == 0 = error "Divided by zero"
    | otherwise = fromIntegral num / fromIntegral den
