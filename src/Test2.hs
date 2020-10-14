{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language TypeFamilies #-}

module Test2 where

import Data.Kind (Type)

type family Product (a :: Type) (b :: Type) :: Type where
    Product a b = (a, b)

data Arrow a b where
    Pure :: (a -> b) -> Arrow a b
    Zip
        :: Arrow a b
        -> Arrow c d
        -> Arrow (Product a c) (Product b d)

interpret :: Arrow a b -> a -> b
interpret = \case
    Pure f -> f
    Zip f g -> interpretZip (interpret f) (interpret g)

interpretZip :: (a -> b) -> (c -> d) -> Product a c -> Product b d
interpretZip f g (a, c) = (f a, g c)
