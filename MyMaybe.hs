-- Example defining Maybe functor, applicative, monad and monoid.

data MyMaybe a = MyJust a | MyNothing
    deriving Show

instance Functor MyMaybe where
    fmap f (MyJust a) = MyJust (f a)
    fmap f MyNothing  = MyNothing

instance Applicative MyMaybe where
    (<*>) (MyJust f) (MyJust b) = MyJust (f b)
    (<*>) MyNothing (MyJust b) = MyNothing

    pure = MyJust

instance Monad MyMaybe where
    (>>=) (MyJust a) f = f a
    (>>=) MyNothing f = MyNothing

instance Semigroup a => Semigroup (MyMaybe a) where
    (<>) (MyJust x) (MyJust y) = MyJust (x <> y)
    (<>) _ _ = MyNothing

instance Monoid a => Monoid (MyMaybe a) where
    mempty = MyNothing
