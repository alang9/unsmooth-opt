{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Optimisation.Brent
       ( search
       , searchWithBounds
       , searchWithBracket
       , searchWithBracketUntil
       , searchWithBracketUntil'
       , searchUntil
       , steps
       ) where

import Control.Arrow
import Control.Monad
import Data.List

import Numeric.Optimisation.Bracket
import Numeric.Optimisation.Internal

-- | Brent's method for minimization via parabolic interpolation and
-- secant rule (golden search) but not bisection
stepsWithBracket :: forall a. (RealFloat a, Ord a) => (a -> a)
                 -> ((a, a), (a, a), (a, a)) -> [((a, a), (a, a), (a, a))]
stepsWithBracket f start@(_, bxfbx, _) = (reorder start:) $
    unfoldr (Just . (fst &&& id) . go) (reorder start, (0, 0, bxfbx, bxfbx))
  where
    reorder (a, b, c)
        | c < a = (c, b, a)
        | otherwise = (a, b, c)
    go :: (((a, a), (a, a), (a, a)), (a, a, (a, a), (a, a)))
       -> (((a, a), (a, a), (a, a)), (a, a, (a, a), (a, a)))
    go (((a, fa), (x, fx), (b, fb)), (d, e, (v, fv), (w, fw)))
        | fu < fx && u >= x = finalize (x, fx) (u, fu) (b, fb) (w, fw) (x, fx)
        | fu < fx = finalize (a, fa) (u, fu) (x, fx) (w, fw) (x, fx)
        | (fu <= fw || w == x) && u < x =
            finalize (u, fu) (x, fx) (b, fb) (w, fw) (u, fu)
        | (fu <= fw || w == x) =
            finalize (a, fa) (x, fx) (u, fu) (w, fw) (u, fu)
        | (fu <= fv || v == x || v == w) && u < x =
            finalize (u, fu) (x, fx) (b, fb) (u, fu) (w, fw)
        | (fu <= fv || v == x || v == w) =
            finalize (a, fa) (x, fx) (u, fu) (u, fu) (w, fw)
        | otherwise = finalize (a, fa) (x, fx) (b, fb) (v, fv) (w, fw)
      where
        (u, fu) = (id &&& f) $ x + d
        zeps = join (*) defaultTolerance * 1e-3
        tol1 = defaultTolerance * abs x + zeps
        p = (if q - r > 0 then negate else id) $ (x - v) * q - (x - w) * r
        q = (x - v) * (fx - fw)
        q2 = abs $ 2 * (q - r)
        r = (x - w) * (fx - fv)
        finalize x1 x2 x3 x4 x5 = ((x1, x2, x3), (d', e', x4, x5))
        (d', e') | abs e > tol1 && abs p < abs (q2 * e / 2) && p > q2 * (a - x)
                   && p < q2 * (b - x) = (p / q2, d) -- Parabolic interpolation
                 | otherwise = ((cgold *) &&& id) $
                   if x >= (a + b) / 2 then a - x else b - x -- Golden Section
        cgold = (3 - sqrt 5) / 2 -- 2 - phi, where phi is the golden ratio

steps :: (RealFloat a, Ord a) => (a -> a) -> a -> [((a, a), (a, a), (a, a))]
steps f x = stepsWithBracket f $ findBracket f x (x + defaultTolerance)

searchWithBracketUntil' :: (RealFloat a, Ord a, Integral c) => a -> Maybe c
                        -> (a -> a) -> ((a, a), (a, a), (a, a)) -> ((a, a), c)
searchWithBracketUntil' tolerance m'itmax f start = (final, numIt)
  where
    (numIt, (_, final, _)) = case takeWhile limit . zip [0..] $ stepsWithBracket f start of
        [] -> (0, start)
        xs -> maybe (last xs) id $ find small xs
    limit (idx, _) = maybe True (idx <=) m'itmax
    small (_, ((x0, _), (x1, _), (x2, _))) =
        abs (x1 - (x0 + x2) / 2) <= tol2 - (x2 - x0) / 2
      where
        zeps = join (*) defaultTolerance * 1e-3
        tol2 = 2 * (tolerance * abs x1 + zeps)

searchWithBracketUntil :: (RealFloat a, Ord a, Integral c) => Maybe c
                       -> (a -> a) -> ((a, a), (a, a), (a, a)) -> ((a, a), c)
searchWithBracketUntil = searchWithBracketUntil' defaultTolerance

searchWithBracket :: (RealFloat a, Ord a, Integral c) => (a -> a)
                  -> ((a, a), (a, a), (a, a)) -> ((a, a), c)
searchWithBracket = searchWithBracketUntil (Just 100)

search :: (RealFloat a, Ord a, Integral c) => (a -> a) -> a -> ((a, a), c)
search f a = searchWithBracket f $ findBracket f a (a + defaultTolerance)

searchUntil :: (RealFloat a, Ord a, Integral c) => Maybe c -> (a -> a) -> a
            -> ((a, a), c)
searchUntil maxIt f a = searchWithBracketUntil maxIt f $
    findBracket f a (a + defaultTolerance)

searchWithBounds :: (RealFloat a, Ord a, Integral c) => (a -> a)
                 -> (Maybe a, Maybe a) -> a -> ((a, a), c)
searchWithBounds f bounds a = searchWithBracket g $
    findBracket g a (a + defaultTolerance)
  where
    g = clamp f bounds
