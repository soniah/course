{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Applicative(
  Applicative(..)
, sequence
, replicateA
, filtering
, return
, fail
) where

import Course.Core
import Course.Apply
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

-- | All instances of the `Applicative` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of left identity
--   `∀x. pure id <*> x ≅ x`
--
-- * The law of right identity
--   `∀x. x <*> pure id ≅ x`
class Apply f => Applicative f where
  pure ::
    a -> f a

-- | Witness that all things with (<*>) and pure also have (<$>).
--
-- >>> (+1) <$> (Id 2)
-- Id 3
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
(<$>) ::
  Applicative f =>
  (a -> b)
  -> f a
  -> f b
(<$>) f = (<*>) $ pure f

-- | Insert into Id.
--
-- prop> pure x == Id x
instance Applicative Id where
  pure ::
    a
    -> Id a
  pure = Id

-- | Insert into a List.
--
-- prop> pure x == x :. Nil
instance Applicative List where
  pure ::
    a
    -> List a
  pure a =
    a :. Nil

-- | Insert into an Optional.
--
-- prop> pure x == Full x
instance Applicative Optional where
  pure ::
    a
    -> Optional a
  pure a =
    Full a

-- | Insert into a constant function.
--
-- prop> pure x y == x
instance Applicative ((->) t) where
  pure ::
    a
    -> ((->) t a)
  pure x _ = x -- ie const

-- | Sequences a list of structures to a structure of list.
--
-- >>> sequence (Id 7 :. Id 8 :. Id 9 :. Nil)
-- Id [7,8,9]
--
-- >>> sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> sequence (Full 7 :. Empty :. Nil)
-- Empty
--
-- >>> sequence (Full 7 :. Full 8 :. Nil)
-- Full [7,8]
--
-- >>> sequence ((*10) :. (+2) :. Nil) 6
-- [60,8]
sequence ::
  Applicative f =>
  List (f a)
  -> f (List a)
sequence xs =
  foldRight (\x acc -> lift2 (:.) x acc) (pure Nil) xs

-- lesson: use normal functions/ideas, but combine
-- into abstraction - that's part of the point of
-- (Applicative) Functors.
--
-- _x :: f a -> f (List a) -> f (List a)
-- _y :: f (List a)

-- foldright of <$> of anon function?
-- (*>) fa fb = (\ _ y -> y) <$> fa <*> fb
-- foldRight :: (a -> b -> b) -> b -> List a -> b

-- (<$>) :: (a -> b) -> f a -> f b   "fmap"
-- (<*>) :: f (a -> b) -> f a -> f b 'apply'
-- pure  :: a -> f a


-- | Replicate an effect a given number of times.
--
-- >>> replicateA 4 (Id "hi")
-- Id ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 (Full "hi")
-- Full ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 Empty
-- Empty
--
-- >>> replicateA 4 (*2) 5
-- [10,10,10,10]
--
-- >>> replicateA 3 ['a', 'b', 'c']
-- ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
replicateA ::
  Applicative f =>
  Int
  -> f a
  -> f (List a)
replicateA n a =
  replicate n <$> a -- "sequence . replicate n" answer is nicer

-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (Id . even) (4 :. 5 :. 6 :. Nil)
-- Id [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil)
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
-- Empty
--
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]
--
-- >>> filtering (const $ True :. True :.  Nil) (1 :. 2 :. 3 :. Nil)
-- [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
--
filtering ::
  Applicative f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
filtering p = foldRight (\x acc -> lift3 doit (p x) (pure x) acc) (pure Nil) where
  doit b x' acc' = if b then x' :. acc' else acc'

-- (<$>) :: Apply f => (a -> b) -> f a -> f b   "fmap"
-- lift2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c
-- <$>, lift2, lift3 are all the same "family"
-- again: use the abstraction to use normal functions

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Applicative IO where
  pure =
    P.return

instance Applicative [] where
  pure =
    P.return

instance Applicative P.Maybe where
  pure =
    P.return

return ::
  Applicative f =>
  a
  -> f a
return =
  pure

fail ::
  Applicative f =>
  Chars
  -> f a
fail =
  error . hlist
