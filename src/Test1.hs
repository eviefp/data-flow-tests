{-# language GADTs #-}
{-# language LambdaCase #-}

module Test1 where

data Arrow a b where
    Pure :: (a -> b) -> Arrow a b
    Zip  :: Arrow a b -> Arrow c d -> Arrow (a, c) (b, d)

interpret :: Arrow a b -> a -> b
interpret = \case
    Pure f -> f
    Zip f g -> interpretZip (interpret f) (interpret g)

interpretZip :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
interpretZip f g (a, c) = (f a, g c)
