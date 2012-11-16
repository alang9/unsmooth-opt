{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Numeric.Optimisation.GoldenSection
    ( findBracket
    , isBracket
    , search
    , searchWithBounds
    , searchWithBracket
    , searchWithBracketUntil
    , searchWithBracketUntil'
    ) where

import Control.Arrow
import Control.Exception

import Numeric.Optimisation.Bracket

searchWithBracket :: (RealFloat a, Ord a, Ord b) =>
    (a -> b) -> ((a, b), (a, b), (a, b)) -> (a, b)
searchWithBracket = searchWithBracketUntil (Just 100)
{-# SPECIALIZE searchWithBracket :: (Double -> Double) ->
    ((Double, Double), (Double, Double), (Double, Double)) ->
    (Double, Double) #-}
{-# SPECIALIZE searchWithBracket :: (Float -> Float) ->
    ((Float, Float), (Float, Float), (Float, Float)) ->
    (Float, Float) #-}

-- | Golden section search, given a bracket and optionally a maximum number
-- of iterations
searchWithBracketUntil :: (RealFloat a, Ord a, Ord b) => Maybe Int -> (a -> b)
                       -> ((a, b), (a, b), (a, b)) -> (a, b)
searchWithBracketUntil = searchWithBracketUntil' defaultTolerance

defaultTolerance :: forall a. RealFloat a => a
defaultTolerance = (fromIntegral (floatRadix (undefined :: a)) **) .
    (/ 2) . fromIntegral . negate $ floatDigits (undefined :: a)
{-# INLINE defaultTolerance #-}

-- | Golden section search, given a bracket, a minimum tolerance to pass and
-- optionally a maximum number of iterations
searchWithBracketUntil' :: (Floating a, Ord a, Ord b) => a -> Maybe Int
                        -> (a -> b) -> ((a, b), (a, b), (a, b)) -> (a, b)
searchWithBracketUntil' tolerance m'itmax f start = final
  where
    ((_, final, _), _) = until stop (step f *** succ) (start, 0)
    stop (fourthPoint f -> ((x1, _), (x2, _), (x3, _), (x4, _)), it) =
        abs (x4 - x1) < tolerance * (abs x2 + abs x3) ||
        maybe True (it >=) m'itmax
{-# SPECIALIZE searchWithBracketUntil' :: Double -> Maybe Int ->
    (Double -> Double) ->
    ((Double, Double), (Double, Double), (Double, Double)) ->
    (Double, Double) #-}
{-# SPECIALIZE searchWithBracketUntil' :: Float -> Maybe Int ->
    (Float -> Float) ->
    ((Float, Float), (Float, Float), (Float, Float)) ->
    (Float, Float) #-}

step :: (Floating a, Ord a, Ord b) => (a -> b)
     -> ((a, b), (a, b), (a, b)) -> ((a, b), (a, b), (a, b))
step f start = assert (isBracket start) . go $ fourthPoint f start
  where
    go (p1, p2, p3, p4)
        | isBracket (p1, p2, p3) = (p1, p2, p3)
        | isBracket (p2, p3, p4) = (p2, p3, p4)
        | otherwise = undefined
{-# SPECIALIZE step :: (Double -> Double) ->
    ((Double, Double), (Double, Double), (Double, Double)) ->
    ((Double, Double), (Double, Double), (Double, Double)) #-}
{-# SPECIALIZE step :: (Float -> Float) ->
    ((Float, Float), (Float, Float), (Float, Float)) ->
    ((Float, Float), (Float, Float), (Float, Float)) #-}

fourthPoint :: (Floating a, Ord a) => (a -> b) -> ((a, b), (a, b), (a, b))
            -> ((a, b), (a, b), (a, b), (a, b))
fourthPoint f ((x1, f1), (x2, f2), (x3, f3)) =
    case abs (x2 - x1) <= abs (x3 - x2) of
        True -> let x4 = goldenSplit x2 x3 in
            ((x1, f1), (x2, f2), (x4, f x4), (x3, f3))
        False -> let x4 = goldenSplit x2 x1 in
            ((x1, f1), (x4, f x4), (x2, f2), (x3, f3))
  where
    phi = (1 + sqrt 5) / 2
    goldenSplit a b = (b + phi * a) / (1 + phi)
{-# SPECIALIZE fourthPoint :: (Double -> Double) ->
    ((Double, Double), (Double, Double), (Double, Double)) ->
    ((Double, Double), (Double, Double), (Double, Double), (Double, Double)) #-}
{-# SPECIALIZE fourthPoint :: (Float -> Float) ->
    ((Float, Float), (Float, Float), (Float, Float)) ->
    ((Float, Float), (Float, Float), (Float, Float), (Float, Float)) #-}

search :: (RealFloat a, Ord a, Show a) => (a -> a) -> a -> (a, a)
search f a = searchWithBracket f $ findBracket f a (a + defaultTolerance)
{-# SPECIALIZE search :: (Double -> Double) -> Double -> (Double, Double) #-}
{-# SPECIALIZE search :: (Float -> Float) -> Float -> (Float, Float) #-}

searchWithBounds :: (RealFloat a, Ord a, Show a) => (a -> a)
                 -> (Maybe a, Maybe a) -> a -> (a, a)
searchWithBounds f (m'lower, m'upper) a = searchWithBracket g $
    findBracket g a (a + defaultTolerance)
  where
    g x | maybe False (x <) m'lower = 1/0
        | maybe False (x >) m'upper = 1/0
        | otherwise = f x
{-- SPECIALIZE searchWithBounds ::
    (Double -> Double) -> Double -> (Double, Double) #-}
{-- SPECIALIZE searchWithBounds ::
    (Float -> Float) -> Float -> (Float, Float) #-}
