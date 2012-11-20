{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Optimisation.Internal
    ( clamp
    , defaultTolerance
    ) where

defaultTolerance :: forall a. RealFloat a => a
defaultTolerance = (fromIntegral (floatRadix (undefined :: a)) **) .
    (/ 2) . fromIntegral . negate $ floatDigits (undefined :: a)
{-# INLINE defaultTolerance #-}

clamp :: (Ord a, Fractional a) => (a -> a) -> (Maybe a, Maybe a) -> a -> a
clamp f (m'lower, m'upper) x
    | maybe False (x <) m'lower = 1/0
    | maybe False (x >) m'upper = 1/0
    | otherwise = f x
{-# SPECIALIZE clamp :: (Double -> Double) -> (Maybe Double, Maybe Double) ->
    Double -> Double #-}
{-# SPECIALIZE clamp :: (Float -> Float) -> (Maybe Float, Maybe Float) ->
    Float -> Float #-}
