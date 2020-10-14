{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}


module GreekLogic where

-- Given a proposition 'HasBeard'
class HasBeard a

-- And a proposition 'IsGreek'
class IsGreek a

-- Then it is true that every Greek has a Beard
instance IsGreek a => HasBeard a

-- And if Socrates exists
data Socrates = Socrates

-- And is a Greek
instance IsGreek Socrates

-- Then we can say he has a beard
implicitly :: c a => ()
implicitly = ()

-- ... implicitly :D
proof :: ()
proof = implicitly @HasBeard @Socrates
