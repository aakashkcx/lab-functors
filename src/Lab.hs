--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Functors                                                              --
--------------------------------------------------------------------------------

module Lab where

import Prelude hiding (Functor(..))

--------------------------------------------------------------------------------

-- | We are definig our own copy of the Functor type class here rather than
-- using the one from Prelude so that we can redefine instances for it that
-- already exist in Prelude.
class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap = map

instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)

--------------------------------------------------------------------------------

-- | This type is a wrapper around values of some type.
data Identity a = Identity a
    deriving (Eq, Show)

-- | 'runIdentity' @action@ extracts the value from @action@.
runIdentity :: Identity a -> a
runIdentity (Identity a) = a

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

--------------------------------------------------------------------------------

data Const v a = Const v
    deriving (Eq, Show)

-- | 'getConst' @action@ extracts the value from @action@.
getConst :: Const v a -> v
getConst (Const x) = x

instance Functor (Const v) where
    fmap f (Const x) = Const (x)

--------------------------------------------------------------------------------

-- | Represents two-dimensional points.
data Point a = Point a a
    deriving (Eq, Show)

instance Functor Point where
    fmap f (Point x y) = Point (f x) (f y)

--------------------------------------------------------------------------------

-- | Represents rose trees.
data RoseTree a = Leaf a | Node [RoseTree a]
    deriving (Eq, Show)

instance Functor RoseTree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node x) = Node (fmap (fmap f) x)

--------------------------------------------------------------------------------

-- | A type that represents values of type @f (g a)@.
data Compose f g a = Compose (f (g a))
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)

--------------------------------------------------------------------------------

-- | A type that wraps around a "stateful" function.
data State s a = St (s -> (a, s))

-- | 'runState' @action initialState@ runs the computation represented by
-- @action@ using the initial state @initialState@.
runState :: State s a -> s -> (a, s)
runState (St m) s = m s

-- | 'fresh' represents a computation which implements the behaviour of a
-- counter that returns the current state and sets the resulting state to the
-- old state incremented by one.
fresh :: State Int Int
fresh = St (\s -> (s,s+1))

instance Functor (State s) where
    -- fmap f (St m) = undefined
    fmap f (St m) = St (\s -> let (x,y) = m s in (f x, y))

--------------------------------------------------------------------------------

instance Functor ((->) r) where
    -- fmap :: (a - b) - (r - a) - (r - b)
    fmap = (.)

--------------------------------------------------------------------------------