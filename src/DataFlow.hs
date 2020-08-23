{-# language GADTs #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
{-# language MultiParamTypeClasses #-}
{-# language KindSignatures #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language TupleSections #-}

module DataFlow where

import Prelude

{-

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
-- ^ Monad Transformer
newtype Star    m a b = Star   { runStar     :: a -> m b }
-- ^ Profunctor

If I want to store a bunch of Star m a bs which I know to compose
(so, essentially, a big composition of Star m a b but without actually
composing them), I can't really escape a GADT and typelevel book-keeping,
right? (and does something like this already exist?)

Ok that was a bad explanation. If I were to describe it constructively,
I'd start from some Star m a a identity, then I would like to be able to
add some Star m a b, then add some Star m b c, etc. However, I need all
of these to be stored in some structure that I interpret later, rather
than composing them.

It's a bit more complicated; I also need the inputs (x) of every new
Star m x y to be a tuple of "all the previous inputs"... so basically
when I add some Star m x y on top of an existing "Star list". So
basically, I would add these in order:

Star m () a
Star m a b
Star m b c
Star m c d


Star m a a
Star m a b
Star m (a, b) c
Star m (a, b, c) d
   -}

data HList t where
    HCons :: a -> HList xs -> HList (a ': xs)
    HNil  :: HList '[]

hlist' :: HList '[ Int, String, Bool ]
hlist' =
    HCons 1
        $ HCons "Hello"
        $ HCons True HNil

getString :: HList '[ Int, String, Bool ] -> String
getString (HCons _ (HCons s _)) = s

data DataFlow t m b where
    DFStart
        :: m a
        -> DataFlow '[] m a
    DFCons
        :: (HList (b ': xs) -> m c)
        -> DataFlow xs m b
        -> DataFlow (b ': xs) m c

-- data MultiFlow where
--     Split
--         :: DataFlow (output ': xs) m output b
--         -> DataFlow (output ': xs) m output c
--         -> MultiFlow

fn0 :: Maybe Int
fn0 = pure 2

fn1 :: HList '[ Int ] -> Maybe String
fn1 (HCons i _) = Just (show i)

fn2 :: HList '[String, Int] -> Maybe Bool
fn2 (HCons s (HCons _ _)) = Just $ s == "1"

interpret :: Monad m => DataFlow xs m b -> m (HList xs, b)
interpret = \case
        DFStart ma -> (HNil,) <$> ma
        DFCons fn prev -> do
           (hl, res) <- interpret prev
           let hlist = HCons res hl
           b <- fn hlist
           pure (hlist, b)

-- | Start a flow with this.
with :: m a -> DataFlow '[] m a
with = DFStart

-- | Compose flows with this.
(~>)
    :: DataFlow xs m b
    -> (HList (b ': xs) -> m c)
    -> DataFlow (b ': xs) m c
(~>) = flip DFCons

-- | Compose flows with Star arrows with this.
(~~>)
    :: forall xs m b c
    .  DataFlow xs m b
    -> (b -> m c)
    -> DataFlow (b ': xs) m c
flow ~~> star = flow ~> promote star
  where
    promote :: (b -> m c) -> (HList (b ': xs) -> m c)
    promote fn (HCons b _) = fn b

dataFlow :: DataFlow '[Bool, String, Int] Maybe String
dataFlow =
    with fn0 ~> fn1 ~> fn2 ~~> (pure . show)


{-
   m1 :: m a
   m2 :: (a)    -> m b
   m3 :: (a, b) -> m c


   interpret $ m1 ~> m2 ~> m3

   splitting streams
   joining streams

                   /------ path1 ~> sink
                  /
  before -------==
                  \
                   \------ path2 ~> opC ------------\
                    \                                === join
                     \---- path3 ~> opA ~> opB -----/

  before :: DataFlow xs             m ()    ouput
  path1  :: DataFlow (output ': xs) m ouput b
  path2  :: DataFlow (output ': xs) m ouput c


  - parallel unrelated
  - splits

-}
