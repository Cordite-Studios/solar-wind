module Solar.Caster.Utility.Delta where

data DeltaContainer a = DeltaContainer
    { content :: a
    , remaining :: !Integer
    } deriving (Show)

instance Eq (DeltaContainer a) where
    a == b = (remaining a) == (remaining b)

instance Ord (DeltaContainer a) where
    compare a b = compare (remaining a) (remaining b)

type Queue a = [DeltaContainer a]

type QueueZipper a = (Queue a, Queue a)

makeZipper :: Queue a -> QueueZipper a
makeZipper a = ([], a)

collectZipper :: Integer -> QueueZipper a -> QueueZipper a
collectZipper _ (o, []) = (o, [])
collectZipper i (o, x:xs)
    | r == 0 = collectZipper i (x:o, xs)
    | r <= i = collectZipper (i - r) (x:o, xs)
    | r > i = (o, x':xs)
    | otherwise = (o, x:xs)
    where
        r = remaining x
        x' = x {remaining = r - i}

sumQueue :: Queue a -> Integer
sumQueue as = sum $ map remaining as

splitWhen' :: (a -> b -> (Bool, b)) -> b -> [a] -> Int -> (Int, b)
splitWhen' _ b [] a = (a, b)
splitWhen' f a (v:vs) r
    | tf = splitWhen' f a' vs (r+1)
    | otherwise = (r, a')
    where (tf, a') = f v a

splitZipper :: Queue a -> DeltaContainer a -> QueueZipper a
splitZipper qs c = (before, prependQueue after c' amt)
    where
        (i, amt) = splitWhen' zipperSplitter rc qs 0
        rc = remaining c
        (before, after) = splitAt i qs
        c' = c {remaining = rc - sumQueue before}

zipperSplitter :: DeltaContainer a -> Integer -> (Bool, Integer)
zipperSplitter v a
    | a == 0    = (False, 0)
    | v' < a    = (True, a - v')
    | v' == a   = (False, 0)
    | otherwise = (False, v')
    where v' = remaining v

prependQueue :: Queue a -> DeltaContainer a -> Integer -> Queue a
prependQueue [] x _ = [x]
prependQueue (o:os) x amt = x:o':os
    where o' = o {remaining = amt}


mergeZipper :: QueueZipper a -> Queue a
mergeZipper (as, bs) = as ++ bs

