{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Numeric.Optimisation.GoldenSection
    ( search
    , searchWithBounds
    , searchWithBracket
    , searchWithBracketUntil
    , searchWithBracketUntil'
    , steps
    , stepsWithBracket
    ) where

import Control.Arrow
import Control.Exception
import Data.List

import Numeric.Optimisation.Bracket
import Numeric.Optimisation.Internal

-- | Golden section search, with a maximum of 100 iterations.
searchWithBracket :: (RealFloat a, Ord a, Ord b, Integral c) => (a -> b)
                  -> ((a, b), (a, b), (a, b)) -> ((a, b), c)
searchWithBracket = searchWithBracketUntil (Just 100)
{-# SPECIALIZE searchWithBracket :: (Double -> Double) ->
    ((Double, Double), (Double, Double), (Double, Double)) ->
    ((Double, Double), Int) #-}
{-# SPECIALIZE searchWithBracket :: (Float -> Float) ->
    ((Float, Float), (Float, Float), (Float, Float)) ->
    ((Float, Float), Int) #-}

-- | Golden section search, given a bracket and optionally a maximum number
-- of iterations.
searchWithBracketUntil :: (RealFloat a, Ord a, Ord b, Integral c) => Maybe c
                       -> (a -> b) -> ((a, b), (a, b), (a, b)) -> ((a, b), c)
searchWithBracketUntil = searchWithBracketUntil' defaultTolerance

-- | Golden section search, given a bracket, a minimum tolerance to pass and
-- optionally a maximum number of iterations.
searchWithBracketUntil' :: (Floating a, Ord a, Ord b, Integral c) => a
                        -> Maybe c -> (a -> b) -> ((a, b), (a, b), (a, b))
                        -> ((a, b), c)
searchWithBracketUntil' tolerance m'itmax f start = (final, numIt)
  where
    (numIt, (_, final, _)) =
        case takeWhile limit . zip [0..] $ stepsWithBracket f start of
            [] -> (0, start)
            xs -> second getBracket . maybe (last xs) id $ find small xs
    limit (idx, _) = maybe True (idx <=) m'itmax
    small (_, ((x1, _), (x2, _), (x3, _), (x4, _))) =
        abs (x4 - x1) < tolerance * (abs x2 + abs x3)
{-# SPECIALIZE searchWithBracketUntil' :: Double -> Maybe Int ->
    (Double -> Double) ->
    ((Double, Double), (Double, Double), (Double, Double)) ->
    ((Double, Double), Int) #-}
{-# SPECIALIZE searchWithBracketUntil' :: Float -> Maybe Int ->
    (Float -> Float) ->
    ((Float, Float), (Float, Float), (Float, Float)) ->
    ((Float, Float), Int) #-}

-- | Produce a list of successive narrowings of an initial bracket using the
-- golden section rule.
stepsWithBracket :: (Floating a, Ord a, Ord b) => (a -> b)
      -> ((a, b), (a, b), (a, b)) -> [((a, b), (a, b), (a, b), (a, b))]
stepsWithBracket f start = assert (isBracket start) .
    iterate (fourthPoint f . getBracket) $ fourthPoint f start
{-# SPECIALIZE stepsWithBracket :: (Double -> Double) ->
    ((Double, Double), (Double, Double), (Double, Double)) ->
    [((Double, Double), (Double, Double), (Double, Double), (Double, Double))] #-}
{-# SPECIALIZE stepsWithBracket :: (Float -> Float) ->
    ((Float, Float), (Float, Float), (Float, Float)) ->
    [((Float, Float), (Float, Float), (Float, Float), (Float, Float))] #-}

steps :: (RealFloat a, Ord a) => (a -> a) -> a -> [((a, a), (a, a), (a, a), (a, a))]
steps f x = stepsWithBracket f $ findBracket f x (x + defaultTolerance)
{-# SPECIALIZE steps :: (Double -> Double) -> Double ->
    [((Double, Double), (Double, Double), (Double, Double), (Double, Double))] #-}
{-# SPECIALIZE steps :: (Float -> Float) -> Float ->
    [((Float, Float), (Float, Float), (Float, Float), (Float, Float))] #-}

getBracket :: (Num a, Ord b, Ord a) => ((a, b), (a, b), (a, b), (a, b))
           -> ((a, b), (a, b), (a, b))
getBracket (p1, p2, p3, p4)
    | isBracket (p1, p2, p3) = (p1, p2, p3)
    | isBracket (p2, p3, p4) = (p2, p3, p4)
    | otherwise = undefined

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

-- | Golden section search with at most 100 iterations, where the second
-- initial point is taken ε from the first.
search :: (RealFloat a, Ord a, Integral c) => (a -> a) -> a -> ((a, a), c)
search f a = searchWithBracket f $ findBracket f a (a + defaultTolerance)
{-# SPECIALIZE search :: (Double -> Double) -> Double ->
    ((Double, Double), Int) #-}
{-# SPECIALIZE search :: (Float -> Float) -> Float -> ((Float, Float), Int) #-}

-- | Golden section search with at most 100 iterations, with (optional) lower
-- and upper bounds, where the second initial point is taken ε from the first.
searchWithBounds :: (RealFloat a, Ord a, Integral c) => (a -> a)
                 -> (Maybe a, Maybe a) -> a -> ((a, a), c)
searchWithBounds f bounds a = searchWithBracket g $
    findBracket g a (a + defaultTolerance)
  where
    g = clamp f bounds
{-- SPECIALIZE searchWithBounds ::
    (Double -> Double) -> Double -> (Double, Double) #-}
{-- SPECIALIZE searchWithBounds ::
    (Float -> Float) -> Float -> (Float, Float) #-}
