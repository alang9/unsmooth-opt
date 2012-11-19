{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Optimisation.Internal
    ( defaultTolerance
    ) where

defaultTolerance :: forall a. RealFloat a => a
defaultTolerance = (fromIntegral (floatRadix (undefined :: a)) **) .
    (/ 2) . fromIntegral . negate $ floatDigits (undefined :: a)
{-# INLINE defaultTolerance #-}
