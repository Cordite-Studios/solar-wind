module Solar.Utility.Delta where

data DeltaContainer a b = DeltaContainer
    { content :: a
    , remaining :: !b
    } deriving (Show)

instance (Eq b) => Eq (DeltaContainer a b) where
    a == b = (remaining a) == (remaining b)

instance (Ord b) => Ord (DeltaContainer a b) where
    compare a b = compare (remaining a) (remaining b)

type Queue a b = [DeltaContainer a b]

type QueueZipper a b = (Queue a b, Queue a b)

makeZipper :: Queue a b -> QueueZipper a b
makeZipper a = ([], a)

sumQueue :: (Num b) => Queue a b -> b
sumQueue as = sum $ map remaining as

splitWhen :: (a -> b -> (Bool, b)) -> b -> [a] -> Int -> (Int, b)
splitWhen _ b [] a = (a, b)
splitWhen f a (v:vs) r
    | tf = splitWhen f a' vs (r+1)
    | otherwise = (r, a')
    where (tf, a') = f v a

insertZipper :: (Num b, Ord b) => Queue a b -> DeltaContainer a b -> QueueZipper a b
insertZipper qs c = (before, c':after)
    where
        rc = remaining c
        (before, after) = zipTo qs rc
        c' = c {remaining = rc - sumQueue before}

zipperSplitter :: (Num b, Ord b) => DeltaContainer a b -> b -> (Bool, b)
zipperSplitter v a
    | a == 0    = (False, 0)
    | v' < a    = (True, a - v')
    | otherwise = (False, a)
    where v' = remaining v

unsplitZipper :: QueueZipper a b -> Queue a b
unsplitZipper (as, bs) = as ++ bs

zipTo :: (Num b, Ord b) => Queue a b -> b -> QueueZipper a b
zipTo qs rc = (before, after')
    where
        (i, amt) = splitWhen zipperSplitter rc qs 0
        (before, after) = splitAt i qs
        after' = case after of
            [] -> []
            (x:xs) -> let x' = x {remaining = (remaining x) - amt} in x':xs 

zipOver :: (Num b, Ord b) => Queue a b -> b -> QueueZipper a b
zipOver qs rc = (before', after)
    where
        (before, after) = zipTo qs rc
        before' = map (\x -> x {remaining = 0}) before

insert :: (Num b, Ord b) => DeltaContainer a b -> Queue a b -> Queue a b
insert c q = unsplitZipper $ insertZipper q c