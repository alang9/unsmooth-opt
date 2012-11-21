module Numeric.Optimisation.Bracket
    ( isBracket
    , findBracket
    ) where

import Control.Arrow
import Control.Monad

isBracket :: (Num a, Ord a, Ord b) => ((a, b), (a, b), (a, b)) -> Bool
isBracket ((x1, f1), (x2, f2), (x3, f3)) =
    ((x2 - x1) * (x3 - x2) > 0 && f2 <= f1 && f2 <= f3)
{-# SPECIALIZE isBracket ::
    ((Double, Double), (Double, Double), (Double, Double)) -> Bool #-}
{-# SPECIALIZE isBracket ::
    ((Float, Float), (Float, Float), (Float, Float)) -> Bool #-}

findBracket :: (Floating a, Ord a) => (a -> a) -> a -> a
            -> ((a, a), (a, a), (a, a))
findBracket f a' b'
    | f b' > f a' = golden (b', f b') (a', f a')
    | otherwise = golden (a', f a') (b', f b')
  where
    golden (a, fa) (b, fb) = go (a, fa) (b, fb) . (id &&& f) $
        b + (1 + sqrt 5) / 2 * (b - a)
    tiny = 1e-20
    glimit = 100
    go (a, fa) (b, fb) (c, fc)
        | isBracket ((a, fa), (b, fb), (c, fc)) = ((a, fa), (b, fb), (c, fc))
        | (b - u) * (u - c) > 0 && fu < fc = ((b, fb), (u, fu), (c, fc))
        | (b - u) * (u - c) > 0 && fu > fb = ((a, fa), (b, fb), (u, fu))
        | any (not . join (<=) . fst) pairs = let x = (id &&& f) (0/0) in (x, x, x)
        | (b - u) * (u - c) > 0 = golden (b, fb) (c, fc)
        | (c - u) * (u - ulim) > 0 && fu < fc = golden (c, fc) (u, fu)
        | (c - u) * (u - ulim) > 0 = go (b, fb) (c, fc) (u, fu)
        | (u - ulim) * (ulim - c) >= 0 = go (b, fb) (c, fc) (ulim, f ulim)
        | otherwise = golden (b, fb) (c, fc)
      where
        pairs = [(a, fa), (b, fb), (c, fc), (u, fu)]
        r = (b - a) * (fb - fc)
        q = (b - c) * (fb - fa)
        fu = f u
        u = b - ((b - c) * q - (b - a) * r) /
            (2 * max (abs (q - r)) tiny * signum (q - r))
        ulim = b + glimit * (c - b)
{-- SPECIALIZE findBracket :: (Double -> Double) -> Double -> Double ->
    ((Double, Double), (Double, Double), (Double, Double)) #-}
{-- SPECIALIZE findBracket :: (Float -> Float) -> Float -> Float ->
    ((Float, Float), (Float, Float), (Float, Float)) #-}
