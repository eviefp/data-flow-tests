{-# language GADTs #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
{-# language MultiParamTypeClasses #-}
{-# language KindSignatures #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language AllowAmbiguousTypes #-}
{-# language PolyKinds #-}
{-# language TypeFamilyDependencies #-}
{-# language PartialTypeSignatures #-}

module Flow where

import Prelude
import Data.Kind (Type)
import DataFlow (HList(..))
import Control.Monad((<=<))

type family (++) (l1 :: [Type]) (l2 :: [Type]) :: [Type] where
  '[] ++ l2 = l2
  l1 ++ '[] = l1
  (x ': xs) ++ l2 = x ': (xs ++ l2)


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
        :: Flow m xs ys
        -> Flow m xs' ys'
        -> Flow m (xs ++ xs') (ys ++ ys')

-------------------------------------------------------------------------------
-- Interpreter

interpret :: Monad m => Flow m as bs -> HList as -> m (HList bs)
interpret flow =
    case flow of
        Identity   -> pure
        Pure inner -> inner
        Compose f g -> interpret f <=< interpret g
        Duplicate   -> pure . duplicateHList
        Discard     -> pure . const HNil
        Zip f g     -> interpret f `zipHList` interpret g

append :: HList as -> HList bs -> HList (as ++ bs)
append HNil bs = bs
append as HNil = as
append (HCons a as) bs = undefined

duplicateHList :: HList as -> HList (as ++ as)
duplicateHList xs = append xs xs

zipHList
    :: (HList xs -> m (HList ys))
    -> (HList xs' -> m (HList ys'))
    -> HList (xs ++ xs')
    -> m (HList (ys ++ ys'))
zipHList = undefined

fst' :: forall m as bs. Flow m (as ++ bs) as
fst' = Zip (Identity :: Flow m as as) (Discard :: Flow m bs '[])

snd' :: forall m as bs. Flow m (as ++ bs) bs
snd' = Zip (Discard :: Flow m as '[]) (Identity :: Flow m bs bs)

(~>)
    :: forall m a b c t
    .  Applicative m
    => Flow m a (t ': b)
    -> Flow m (a ++ b) c
    -> Flow m a (a ++ b ++ c)
left ~> right =
    Zip
        (Identity @m @(a ++ b))
        right
       `Compose` Duplicate
       `Compose`
        Zip
            (Identity @m @a)
            (Pure go `Compose` left)
        `Compose`
            Duplicate
  where
    go :: HList (t ': b) -> m (HList b)
    go (HCons _ rest) = pure rest


-------------------------------------------------------------------------------
-- Examples

step1 :: Flow Maybe '[Int] '[Int, String]
step1 =
    Pure
       $ \(HCons i _) ->
           Just $ HCons i (HCons (show i) HNil)

step2 :: Flow Maybe '[Int, String] '[Bool]
step2 =
    Pure
        $ \(HCons i (HCons s _)) ->
            Just (HCons (show i == s) HNil)

step3 :: Flow Maybe '[Int, String, Bool] '[Int]
step3 =
    Pure
        $ \(HCons _ (HCons _ (HCons b _))) ->
            Just (HCons (if b then 1 else 2) HNil)

step4 :: Flow Maybe '[Int, String, Bool, Int] '[String]
step4 =
    Pure
        $ \(HCons _ (HCons _ (HCons _ (HCons i _)))) ->
            Just $ HCons (show i) HNil

step14 :: Flow Maybe '[Int] '[Int, String, Bool, Int, String]
step14 = step1 ~> step2 ~> step3 ~> step4
