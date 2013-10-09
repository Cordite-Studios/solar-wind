module Solar.Utility.Delta where

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

sumQueue :: Queue a -> Integer
sumQueue as = sum $ map remaining as

splitWhen :: (a -> b -> (Bool, b)) -> b -> [a] -> Int -> (Int, b)
splitWhen _ b [] a = (a, b)
splitWhen f a (v:vs) r
    | tf = splitWhen f a' vs (r+1)
    | otherwise = (r, a')
    where (tf, a') = f v a

insertZipper :: Queue a -> DeltaContainer a -> QueueZipper a
insertZipper qs c = (before, c':after)
    where
        rc = remaining c
        (before, after) = zipTo qs rc
        c' = c {remaining = rc - sumQueue before}

zipperSplitter :: DeltaContainer a -> Integer -> (Bool, Integer)
zipperSplitter v a
    | a == 0    = (False, 0)
    | v' < a    = (True, a - v')
    | otherwise = (False, a)
    where v' = remaining v

unsplitZipper :: QueueZipper a -> Queue a
unsplitZipper (as, bs) = as ++ bs

zipTo :: Queue a -> Integer -> QueueZipper a
zipTo qs rc = (before, after')
    where
        (i, amt) = splitWhen zipperSplitter rc qs 0
        (before, after) = splitAt i qs
        after' = case after of
            [] -> []
            (x:xs) -> let x' = x {remaining = (remaining x) - amt} in x':xs 

zipOver :: Queue a -> Integer -> QueueZipper a
zipOver qs rc = (before', after)
    where
        (before, after) = zipTo qs rc
        before' = map (\x -> x {remaining = 0}) before

insert :: DeltaContainer a -> Queue a -> Queue a
insert c q = unsplitZipper $ insertZipper q c