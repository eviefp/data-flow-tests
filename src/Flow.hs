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

module Flow where

import Prelude
import Control.Monad((<=<))
import Data.Vinyl.Derived (HList)
import Data.Vinyl.Core
import Data.Vinyl.TypeLevel
import qualified Data.Vinyl.Functor as V

-- type family (++) (l1 :: [Type]) (l2 :: [Type]) :: [Type] where
--   '[] ++ l2 = l2
--   l1 ++ '[] = l1
--   (x ': xs) ++ l2 = x ': (xs ++ l2)


-- | 'Flow' represents kleisli arrows from a list of inputs 'as' to a list
-- of outputs 'bs' under a context 'm'.
data Flow m as bs where
    -- | This is the main constructor for injecting kleisli arrows into
    -- the flow type.
    Pure
        :: (HList as -> m (HList bs))
        -> Flow m as bs
    -- | This Identity is conceptually identical with 'Pure pure', but
    -- interpreters would not be able to inspect the functions inside 'Pure'.
    Identity
        :: Flow m as as
    -- | Duplicate/split the input.
    Duplicate
        :: Flow m xs (xs ++ xs)
    -- | ???
    Discard
        :: Flow m xs '[]
    -- | Serially compose two flows.
    Compose
        :: Flow m bs cs
        -> Flow m as bs
        -> Flow m as cs
    -- | Compose flows in parallel.
    Zip
        :: Split xs xs' (xs ++ xs')
        => Flow m xs  ys
        -> Flow m xs' ys'
        -> Flow m (xs ++ xs') (ys ++ ys')

{-
                            /----- f ------ f'
-- f0 -- f1 -- Duplicate -==  Zip     Zip
                            \----- g ------ g'

a -> m b
(a, b) -> m c
(a, b, c) -> m d
(a, b, c, d) -> m e
... etc

-}

-------------------------------------------------------------------------------
-- Interpreter

-- instance is ~ RImage xs xs => RecSubset Rec xs (xs ++ ys) is where
--     rsubsetC = undefined

-- class Subset all sub where
--     subset :: HList all -> HList sub

-- instance Subset all '[] where
--     subset _ = RNil

-- instance Subset all sub => Subset (x ': all) (x ': sub) where
--    subset (x :& rest) = x :& subset rest

class Split xs ys all | all xs -> ys where
    first :: HList all -> HList xs
    second :: HList all -> HList ys

instance Split xs ys all => Split (x ': xs) ys (x ': all) where
    first (x :& rest) = x :& first rest
    second (_ :& rest) = second @xs rest

instance Split '[] ys ys where
    first _ = RNil
    second = id

interpret :: Monad m => Flow m as bs -> HList as -> m (HList bs)
interpret flow =
    case flow of
        Identity    -> pure
        Pure inner  -> inner
        Compose f g -> interpret f <=< interpret g
        Duplicate   -> pure . duplicateHList
        Discard     -> pure . const RNil
        Zip f g     -> interpret f `zipHList` interpret g

duplicateHList :: HList as -> HList (as ++ as)
duplicateHList xs = xs <+> xs

zipHList
    :: forall m xs ys xs' ys'
    .  Monad m
    => Split xs xs' (xs ++ xs')
    => (HList xs -> m (HList ys))
    -> (HList xs' -> m (HList ys'))
    -> HList (xs ++ xs')
    -> m (HList (ys ++ ys'))
zipHList f g hlist = do
    left <- f (first hlist)
    right <- g (second @xs hlist)
    pure $ left <+> right

-- fst' :: forall m as bs. Flow m (as ++ bs) as
-- fst' = Zip (Identity :: Flow m as as) (Discard :: Flow m bs '[])

-- snd' :: forall m as bs. Flow m (as ++ bs) bs
-- snd' = Zip (Discard :: Flow m as '[]) (Identity :: Flow m bs bs)

-- | Composition of functions as I require for my DSL.
(~>)
    :: forall m a b c t
    .  Applicative m
    => Split (a ++ b) (a ++ b) ((a ++ b) ++ (a ++ b))
    => Split a a (a ++ a)
    => Flow m a (t ': b)
             -- ^ try replacing ': with ++
    -> Flow m (a ++ b) c
    -> Flow m a (a ++ b ++ c)
left ~> right =
    (Zip
        (Identity @m @(a ++ b))
        -- Flow m (a ++ b) (a ++ b)
        right
        -- Flow m (a ++ b) c
        :: Flow m ((a ++ b) ++ (a ++ b)) ((a ++ b) ++ c)
    )
    -- Flow m ((a ++ b) ++ (a ++ b)) ((a ++ b) ++ c)
   `Compose`
       (Duplicate @m @(a ++ b))
           -- Flow m (a ++ b) ((a ++ b) ++ (a ++ b))
        -- Flow m (a ++ b) ((a ++ b) ++ c)
       `Compose`
            (Zip
                (Identity @m @a)
                -- Flow m a a
                (Pure go
                    -- Flow m (t ': b) b
                     `Compose`
                          left
                    -- Flow m a (t ': b)
                )
                -- Flow m a b
            )
            -- Flow m (a ++ a) (a ++ b)
           `Compose`
                (Duplicate @m @a)
                -- Flow m a (a ++ a)
            -- Flow a (a ++ b)
  where
    go :: HList (t ': b) -> m (HList b)
    go (_ :& rest) = pure rest


-------------------------------------------------------------------------------
-- Examples

step1 :: Flow Maybe '[Int] '[Int, String]
step1 =
    Pure
       $ \(i :& _) ->
           Just $ i :& pure (show i) :& RNil

-- TODO FIX THIS HERE
step1' :: Flow Maybe '[Int] '[String]
step1' = undefined

step2 :: Flow Maybe '[Int, String] '[Bool]
step2 =
    Pure
        $ \(V.Identity i :& V.Identity s :& _) ->
            Just (pure (show i == s) :& RNil)

step3 :: Flow Maybe '[Int, String, Bool] '[Int]
step3 =
    Pure
        $ \(_ :& _ :& (V.Identity b) :& _) ->
            Just (pure (if b then 3 else 4) :& RNil)

step4 :: Flow Maybe '[Int, String, Bool, Int] '[String]
step4 =
    Pure
        $ \(_ :& _ :& _ :& V.Identity i :& _) ->
            Just $ pure (show i) :& RNil

step14 :: Flow Maybe '[Int] '[Int, String, Bool, Int, String]
step14 = step1 ~> step2 ~> step3 ~> step4

result :: Maybe (HList '[Int, String, Bool, Int, String])
result = interpret step14 (pure 1 :& RNil)
