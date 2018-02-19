module CompMonoid where

newtype Comp a = Comp { applyComp :: (a -> a) }

instance Monoid (Comp a) where
    mempty = Comp id
    mappend (Comp f) (Comp g) = Comp $ g . f
