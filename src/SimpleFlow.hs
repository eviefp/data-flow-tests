{-# language GADTs #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
{-# language MultiParamTypeClasses #-}
{-# language KindSignatures #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language AllowAmbiguousTypes #-}
{-# language PolyKinds #-}
{-# language TypeFamilyDependencies #-}
{-# language PartialTypeSignatures #-}
{-# language FunctionalDependencies #-}
{-# language UndecidableInstances #-}

module SimpleFlow where

import Data.Bool (bool)
import Prelude
import Control.Monad((<=<))

-- TODO: Add a 'p' parameter for the 'Pure' ctor.
data Flow m a b where
    -- | This is the main constructor for injecting kleisli arrows into
    -- the flow type.
    Pure
        -- p as bs
        :: (a -> m b)
        -> Flow m a b
    -- | This Identity is conceptually identical with 'Pure pure', but
    -- interpreters would not be able to inspect the functions inside 'Pure'.
    Identity
        :: Flow m a a
    -- | Duplicate/split the input.
    Duplicate
        :: Flow m a (a, a)
    -- | ???
    Discard
        :: Flow m xs ()
    -- | Serially compose two flows.
    Compose
        :: Flow m b c
        -> Flow m a b
        -> Flow m a c
    -- | Compose flows in parallel.
    Zip
        :: Flow m a b
        -> Flow m c d
        -> Flow m (a, c) (b, d)


-------------------------------------------------------------------------------
-- Interpreter

interpret :: Monad m => Flow m a b -> a -> m b
interpret flow =
    case flow of
        Identity    -> pure
        Pure inner  -> inner
        Compose f g -> interpret f <=< interpret g
        Duplicate   -> pure . (\a -> (a, a))
        Discard     -> pure . const ()
        Zip f g     -> interpret f `combine` interpret g

combine :: Applicative m => (a -> m b) -> (c -> m d) -> (a, c) -> m (b, d)
combine f g (a, c) = (,) <$> f a <*> g c

-- fst' :: forall m as bs. Flow m (as ++ bs) as
-- fst' = Zip (Identity :: Flow m as as) (Discard :: Flow m bs '[])

-- snd' :: forall as bs m. Split as bs (as ++ bs) => Flow m (as ++ bs) bs
-- snd' = Zip (Discard :: Flow m as '[]) (Identity :: Flow m bs bs)

-- | Composition of functions as I require for my DSL.
(~>)
    :: forall m a b c
    .  Flow m a b
    -> Flow m b c
    -> Flow m a c
(~>) = flip Compose


-------------------------------------------------------------------------------
-- Examples

-- data AppF a
--    = GetWhateverFromDb
--    | ...

step1 :: Flow Maybe Int String
step1 = Pure (pure . show)

step2 :: Flow Maybe String Bool
step2 = Pure (pure . (== "1"))

step3 :: Flow Maybe Bool Int
step3 = Pure (pure . (bool 0 1))

step4 :: Flow Maybe Int String
step4 = Pure (pure . show)

step14 :: Flow Maybe Int String
step14 = step1 ~> step2 ~> step3 ~> step4

result :: Int -> Maybe String
result = interpret step14
