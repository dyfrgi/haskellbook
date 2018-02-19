module WeirdMonoid where

import Data.Monoid

newtype Mem s a =
    Mem {
        runMem :: s -> (a, s)
        }

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend ml mr = Mem $ combine
        where combine s =
                let (al, sl) = runMem ml $ s    -- order operations left to right
                    (ar, sr) = runMem mr $ sl
                in (mappend al ar, sr)          -- combine a left to right, too

f',g',h' :: Mem Integer String
f' = Mem $ \s -> ("hello", s + 1)
g' = Mem $ \s -> ("world", s * 2)
h' = Mem $ \s -> ("blarg", s - 8)

main :: IO ()
main = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmright
    print $ rmleft 
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0
    print $ runMem (mconcat [f', g']) $ 3
    print $ runMem (mconcat [f', g', h']) $ 3
