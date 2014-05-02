{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}

module Data.List.Concat where

import           Control.Applicative
import           Control.Monad
import           Data.Bool
import           Data.Eq
import           Data.Foldable (Foldable)
import qualified Data.Foldable as F
import           Data.Function
import           Data.Int
import qualified Data.List as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.String hiding (unlines, unwords)
import           Data.Traversable
import           Data.Tuple
import           Prelude (Num, Integral, (+), (-), undefined, error)
import           Text.Show

data Concat a = Whole [a]
              | Split (Concat a) (Concat a)
    deriving (Functor, Foldable, Traversable)

instance Show a => Show (Concat a) where
    show (Whole xs)    = "Whole " L.++ show xs
    show (Split xs ys) = "Split (" L.++ show xs L.++ " " L.++ show ys L.++ ")"

instance Applicative Concat where
    pure x = Whole [x]
    Whole fs <*> Whole xs = Whole (fs <*> xs)
    Split fs gs <*> Split xs ys =
        Split (Split (fs <*> xs) (fs <*> ys))
              (Split (gs <*> xs) (gs <*> ys))
    fs <*> Split xs ys = Split (fs <*> xs) (fs <*> ys)
    Split fs gs <*> xs = Split (fs <*> xs) (gs <*> xs)

instance Monad Concat where
    return = pure
    Whole xs >>= f =
        L.foldr (\x r -> case r of
                      Whole [] -> f x
                      _ -> Split (f x) r) (Whole []) xs
    Split xs ys >>= f = Split (xs >>= f) (ys >>= f)

instance Monoid (Concat a) where
    mempty  = Whole []
    Whole [] `mappend` xs = xs
    xs `mappend` Whole [] = xs
    xs `mappend` ys = Split xs ys

-- | Case analyze a concat type's head and tail.
uncons :: Concat a -> Maybe (a, Concat a)
uncons (Whole [])     = Nothing
uncons (Whole (x:xs)) = Just (x, Whole xs)
uncons (Split xs ys)  = case uncons xs of
    Nothing -> uncons ys
    Just (z, zs) -> Just (z, zs <> ys)

-- | Case analyze a concat type's head and tail.
caseConcat :: b -> (a -> Concat a -> b) -> Concat a -> b
caseConcat b f xs = maybe b (uncurry f) (uncons xs)

cons :: a -> Concat a -> Concat a
cons x (Whole xs) = Whole (x:xs)
cons x (Split xs ys) = Split (cons x xs) ys

snoc :: Concat a -> a -> Concat a
snoc xs@(Whole _) x = Split xs (pure x)
snoc (Split xs ys) x = Split xs (snoc ys x)

-- | Remove any empty lists which may have accumulated inside the Concat.
compact :: Concat a -> Concat a
compact (Split xs ys) = compact xs <> compact ys
compact xs = xs

toList :: Concat a -> [a]
toList = F.toList

fromList :: [a] -> Concat a
fromList = Whole

head :: Concat a -> a
head = fromMaybe (error "Data.List.Concat.head: empty list") . headMay

headMay :: Concat a -> Maybe a
headMay (Whole []) = Nothing
headMay (Whole (x:_)) = Just x
headMay (Split xs ys) = headMay xs <|> headMay ys

init :: Concat a -> Concat a
init = fromMaybe (error "Data.List.Concat.init: empty list") . initMay

initMay :: Concat a -> Maybe (Concat a)
initMay (Whole []) = Nothing
initMay (Whole xs) = Just $ Whole $ L.init xs
initMay (Split (Whole []) xs) = initMay xs
initMay (Split xs (Whole [])) = initMay xs
initMay (Split xs ys) = Split xs <$> initMay ys

tail :: Concat a -> Concat a
tail = fromMaybe (error "Data.List.Concat.tail: empty list") . tailMay

tailMay :: Concat a -> Maybe (Concat a)
tailMay (Whole []) = Nothing
tailMay (Whole xs) = Just $ Whole $ L.tail xs
tailMay (Split (Whole []) xs) = tailMay xs
tailMay (Split xs (Whole [])) = tailMay xs
tailMay (Split xs ys) = flip Split ys <$> tailMay xs

last :: Concat a -> a
last = fromMaybe (error "Data.List.Concat.last: empty list") . lastMay

lastMay :: Concat a -> Maybe a
lastMay (Whole []) = Nothing
lastMay (Whole xs) = Just $ L.last xs
lastMay (Split xs ys) = lastMay ys <|> lastMay xs

null :: Concat a -> Bool
null (Whole []) = True
null (Whole _) = False
null (Split xs ys) = null xs || null ys

length :: Concat a -> Int
length (Whole xs)    = L.length xs
length (Split xs ys) = length xs + length ys

map :: (a -> b) -> Concat a -> Concat b
map = fmap

reverse :: Concat a -> Concat a
reverse (Whole xs) = Whole (L.reverse xs)
reverse (Split xs ys) = Split (reverse ys) (reverse xs)

intersperse :: a -> Concat a -> Concat a
intersperse a (Whole xs) = Whole (L.intersperse a xs)
intersperse a (Split xs ys) =
    Split (intersperse a xs) (Split (pure a) (intersperse a ys))

intercalate :: Concat a -> Concat (Concat a) -> Concat a
intercalate xs xss = concat (intersperse xs xss)

transpose :: Concat (Concat a) -> Concat (Concat a)
transpose = Whole . L.map Whole . L.transpose . L.map F.toList . F.toList

subsequences :: Concat a -> Concat (Concat a)
subsequences = Whole . L.map Whole . L.subsequences . F.toList

permutations :: Concat a -> Concat (Concat a)
permutations = Whole . L.map Whole . L.permutations . F.toList

concat :: Concat (Concat a) -> Concat a
concat = (>>= id)

concatMap :: (a -> Concat b) -> Concat a -> Concat b
concatMap = flip (>>=)

and :: Concat Bool -> Bool
and = F.and

or :: Concat Bool -> Bool
or = F.or

any :: (a -> Bool) -> Concat a -> Bool
any = F.any

all :: (a -> Bool) -> Concat a -> Bool
all = F.all

sum :: Num a => Concat a -> a
sum = F.sum

product :: Num a => Concat a -> a
product = F.product

maximum :: Ord a => Concat a -> a
maximum = F.maximum

minimum :: Ord a => Concat a -> a
minimum = F.minimum

scanl' :: (a -> b -> a) -> a -> Concat b -> (Concat a, a)
scanl' _ q (Whole []) = (Whole [], q)
scanl' f q (Whole [x]) = (pure q, f q x)
scanl' f q (Whole (x:xs)) =
    let (c, a) = scanl' f (f q x) (Whole xs)
    in (cons q c, a)
scanl' f q (Split xs ys) =
    let (c,  a)  = scanl' f q xs
        (c', a') = scanl' f a ys
    in (c <> c', a')

scanl :: (a -> b -> a) -> a -> Concat b -> Concat a
scanl f q xs = uncurry snoc $ scanl' f q xs

scanl1 :: (a -> a -> a) -> Concat a -> Concat a
scanl1 f = caseConcat mempty (scanl f)

scanr :: (a -> b -> b) -> b -> Concat a -> Concat b
scanr f q0 xs = case uncons xs of
    Nothing -> Whole [q0]
    Just (x, xs') -> f x q `cons` qs
      where qs@(uncons -> Just (q, _)) = scanr f q0 xs'

scanr1 :: (a -> a -> a) -> Concat a -> Concat a
scanr1 = undefined

mapAccumL :: (acc -> x -> (acc, y)) -> acc -> Concat x -> (acc, Concat y)
mapAccumL = undefined

mapAccumR :: (acc -> x -> (acc, y)) -> acc -> Concat x -> (acc, Concat y)
mapAccumR = undefined

iterate :: (a -> a) -> a -> Concat a
iterate = undefined

repeat :: a -> Concat a
repeat = Whole . L.repeat

replicate :: Int -> a -> Concat a
replicate n a = Whole $ L.replicate n a

cycle :: Concat a -> Concat a
cycle xs = Split xs (cycle xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> Concat a
unfoldr f b = Whole $ L.unfoldr f b

takeMay :: Int -> Concat a -> Maybe (Concat a)
takeMay n xs = case splitAt n xs of
    (Whole [], _) -> Nothing
    (ys, _)       -> Just ys

dropMay :: Int -> Concat a -> Concat a
dropMay n xs = snd $ splitAt n xs

splitAt :: Int -> Concat a -> (Concat a, Concat a)
splitAt m ws = fst $ go m ws
  where
    go n (Whole xs) | n <= 0 = ((Whole [], Whole xs), 0)
    go _ (Whole []) = ((Whole [], Whole []), 0)
    go n (Whole (x:xs)) =  ((cons x xs', xs''), z+1) where
        ((xs', xs''), z) = go (n-1) (Whole xs)
    go n (Split xs ys) =
        let (zs, z) = go n xs
            (zs', z') = go (n - z) ys
        in (zs <> zs', z + z')

splitBy :: (a -> Bool) -> Concat a -> (Concat a, Concat a)
splitBy g zs = fst $ go g zs
  where
    go _ (Whole []) = ((Whole [], Whole []), True)
    go p ts@(Whole (x:xs))
        | p x = let ((z, z'), t) = go p (Whole xs)
                in ((x `cons` z, z'), t)
        | otherwise = ((Whole [], ts), False)
    go p (Split xs ys) =
        let ((z, z'), t) = go p xs
        in if t
           then let ((w, w'), t') = go p ys
                in ((z <> w, z' <> w'), t')
           else ((z, z' <> ys), t)

takeWhile :: (a -> Bool) -> Concat a -> Concat a
takeWhile f xs = fst $ splitBy f xs

dropWhile :: (a -> Bool) -> Concat a -> Concat a
dropWhile f xs = snd $ splitBy f xs

dropWhileEnd :: (a -> Bool) -> Concat a -> Concat a
dropWhileEnd p =
    F.foldr
        (\x xs -> if p x && null xs then Whole [] else x `cons` xs)
        (Whole [])

span :: (a -> Bool) -> Concat a -> (Concat a, Concat a)
span = undefined

break :: (a -> Bool) -> Concat a -> (Concat a, Concat a)
break = undefined

stripPrefix :: Eq a => Concat a -> Concat a -> Maybe (Concat a)
stripPrefix (Whole []) ys = Just ys
stripPrefix (uncons -> Just (x, xs)) (uncons -> Just (y, ys))
    | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

group :: Eq a => Concat a -> Concat (Concat a)
group = undefined

inits :: Concat a -> Concat (Concat a)
inits (Whole xs) = Whole $ L.map Whole (L.inits xs)
inits (Split xs ys) =
    let zs = inits xs
    in zs <> map (last zs <>) (tail (inits ys))

tails :: Concat a -> Concat (Concat a)
tails xs = xs `cons` caseConcat (Whole []) (const tails) xs

isPrefixOf :: Eq a => Concat a -> Concat a -> Bool
isPrefixOf ws zs = caseConcat True go ws
  where
    go x xs = caseConcat False go' zs
      where
        go' y ys = x == y && isPrefixOf xs ys

isSuffixOf :: Eq a => Concat a -> Concat a -> Bool
isSuffixOf x y =  reverse x `isPrefixOf` reverse y

isInfixOf :: Eq a => Concat a -> Concat a -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

elem :: Eq a => a -> Concat a -> Bool
elem = F.elem

notElem :: Eq a => a -> Concat a -> Bool
notElem = F.notElem

lookup :: Eq a => a -> Concat (a, b) -> Maybe b
lookup a (Whole xs)    = L.lookup a xs
lookup a (Split xs ys) = lookup a xs <|> lookup a ys

find :: (a -> Bool) -> Concat a -> Maybe a
find = F.find

filter :: (a -> Bool) -> Concat a -> Concat a
filter f (Whole xs)    = Whole (L.filter f xs)
filter f (Split xs ys) = filter f xs <> filter f ys

partition :: (a -> Bool) -> Concat a -> (Concat a, Concat a)
partition = undefined

(!!) :: Concat a -> Int -> a
(!!) = undefined

elemIndex :: Eq a => a -> Concat a -> Maybe Int
elemIndex x = findIndex (x ==)

elemIndices :: Eq a => a -> Concat a -> Concat Int
elemIndices x = findIndices (x ==)

findIndex :: (a -> Bool) -> Concat a -> Maybe Int
findIndex p = listToMaybe . F.toList . findIndices p

findIndices :: (a -> Bool) -> Concat a -> Concat Int
findIndices = undefined

zip :: Concat a -> Concat b -> Concat (a, b)
zip = zipWith (,)

zip3 :: Concat a -> Concat b -> Concat c -> Concat (a, b, c)
zip3 = zipWith3 (,,)

zip4 :: Concat a -> Concat b -> Concat c -> Concat d -> Concat (a, b, c, d)
zip4 = zipWith4 (,,,)

zip5 :: Concat a -> Concat b -> Concat c -> Concat d -> Concat e
     -> Concat (a, b, c, d, e)
zip5 = zipWith5 (,,,,)

zip6 :: Concat a -> Concat b -> Concat c -> Concat d -> Concat e -> Concat f
     -> Concat (a, b, c, d, e, f)
zip6 = zipWith6 (,,,,,)

zip7 :: Concat a
     -> Concat b
     -> Concat c
     -> Concat d
     -> Concat e
     -> Concat f
     -> Concat g
     -> Concat (a, b, c, d, e, f, g)
zip7 = zipWith7 (,,,,,,)

zipWith :: (a -> b -> c) -> Concat a -> Concat b -> Concat c
zipWith z
    (uncons -> Just (a, as))
    (uncons -> Just (b, bs))
    = z a b `cons` zipWith z as bs
zipWith _ _ _ = Whole []

zipWith3 :: (a -> b -> c -> d) -> Concat a -> Concat b -> Concat c -> Concat d
zipWith3 z
    (uncons -> Just (a, as))
    (uncons -> Just (b, bs))
    (uncons -> Just (c, cs))
    = z a b c `cons` zipWith3 z as bs cs
zipWith3 _ _ _ _ = Whole []

zipWith4 :: (a -> b -> c -> d -> e)
         -> Concat a
         -> Concat b
         -> Concat c
         -> Concat d
         -> Concat e
zipWith4 z
    (uncons -> Just (a, as))
    (uncons -> Just (b, bs))
    (uncons -> Just (c, cs))
    (uncons -> Just (d, ds))
    = z a b c d `cons` zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _ = Whole []

zipWith5 :: (a -> b -> c -> d -> e -> f)
         -> Concat a
         -> Concat b
         -> Concat c
         -> Concat d
         -> Concat e
         -> Concat f
zipWith5 z
    (uncons -> Just (a, as))
    (uncons -> Just (b, bs))
    (uncons -> Just (c, cs))
    (uncons -> Just (d, ds))
    (uncons -> Just (e, es))
    = z a b c d e `cons` zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _ = Whole []

zipWith6 :: (a -> b -> c -> d -> e -> f -> g)
         -> Concat a
         -> Concat b
         -> Concat c
         -> Concat d
         -> Concat e
         -> Concat f
         -> Concat g
zipWith6 z
    (uncons -> Just (a, as))
    (uncons -> Just (b, bs))
    (uncons -> Just (c, cs))
    (uncons -> Just (d, ds))
    (uncons -> Just (e, es))
    (uncons -> Just (f, fs))
    = z a b c d e f `cons` zipWith6 z as bs cs ds es fs
zipWith6 _ _ _ _ _ _ _ = Whole []

zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h)
         -> Concat a
         -> Concat b
         -> Concat c
         -> Concat d
         -> Concat e
         -> Concat f
         -> Concat g
         -> Concat h
zipWith7 z
    (uncons -> Just (a, as))
    (uncons -> Just (b, bs))
    (uncons -> Just (c, cs))
    (uncons -> Just (d, ds))
    (uncons -> Just (e, es))
    (uncons -> Just (f, fs))
    (uncons -> Just (g, gs))
    = z a b c d e f g `cons` zipWith7 z as bs cs ds es fs gs
zipWith7 _ _ _ _ _ _ _ _ = Whole []

unzip :: Concat (a, b) -> (Concat a, Concat b)
unzip = F.foldr
             (\(a,b) ~(as,bs) ->
               (a `cons` as,
                b `cons` bs))
             (Whole [],Whole [])

unzip3 :: Concat (a, b, c) -> (Concat a, Concat b, Concat c)
unzip3 = F.foldr
             (\(a,b,c) ~(as,bs,cs) ->
               (a `cons` as,
                b `cons` bs,
                c `cons` cs))
             (Whole [],Whole [],Whole [])

unzip4 :: Concat (a, b, c, d) -> (Concat a, Concat b, Concat c, Concat d)
unzip4 = F.foldr
             (\(a,b,c,d) ~(as,bs,cs,ds) ->
               (a `cons` as,
                b `cons` bs,
                c `cons` cs,
                d `cons` ds))
             (Whole [],Whole [],Whole [],Whole [])

unzip5 :: Concat (a, b, c, d, e)
       -> (Concat a, Concat b, Concat c, Concat d, Concat e)
unzip5 = F.foldr
             (\(a,b,c,d,e) ~(as,bs,cs,ds,es) ->
               (a `cons` as,
                b `cons` bs,
                c `cons` cs,
                d `cons` ds,
                e `cons` es))
             (Whole [],Whole [],Whole [],Whole [],Whole [])

unzip6 :: Concat (a, b, c, d, e, f)
       -> (Concat a, Concat b, Concat c, Concat d, Concat e, Concat f)
unzip6 = F.foldr
             (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) ->
               (a `cons` as,
                b `cons` bs,
                c `cons` cs,
                d `cons` ds,
                e `cons` es,
                f `cons` fs))
             (Whole [],Whole [],Whole [],Whole [],Whole [],Whole [])

unzip7 :: Concat (a, b, c, d, e, f, g)
       -> (Concat a, Concat b, Concat c, Concat d, Concat e, Concat f, Concat g)
unzip7 = F.foldr
             (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) ->
               (a `cons` as,
                b `cons` bs,
                c `cons` cs,
                d `cons` ds,
                e `cons` es,
                f `cons` fs,
                g `cons` gs))
             (Whole [],Whole [],Whole [],Whole [],Whole [],Whole [],Whole [])

lines :: String -> Concat String
lines = Whole . L.lines

words :: String -> Concat String
words = Whole . L.words

unlines :: Concat String -> String
unlines = caseConcat [] (\l ls -> l L.++ '\n' : unlines ls)

unwords :: Concat String -> String
unwords = caseConcat [] (\l ls -> l L.++ ' ' : unwords ls)

nub :: Eq a => Concat a -> Concat a
nub = undefined

delete :: Eq a => a -> Concat a -> Concat a
delete = deleteBy (==)

(\\) :: Eq a => Concat a -> Concat a -> Concat a
(\\) = F.foldl (flip delete)

union :: Eq a => Concat a -> Concat a -> Concat a
union = unionBy (==)

intersect :: Eq a => Concat a -> Concat a -> Concat a
intersect = intersectBy (==)

sort :: Ord a => Concat a -> Concat a
sort = sortBy compare

insert :: Ord a => a -> Concat a -> Concat a
insert = insertBy compare

nubBy :: (a -> a -> Bool) -> Concat a -> Concat a
nubBy = undefined

deleteBy :: (a -> a -> Bool) -> a -> Concat a -> Concat a
deleteBy _  _ (Whole []) = Whole []
deleteBy eq x (Whole xs) = Whole (L.deleteBy eq x xs)
deleteBy eq x (Split xs ys) = deleteBy eq x xs <> deleteBy eq x ys

deleteFirstsBy :: (a -> a -> Bool) -> Concat a -> Concat a -> Concat a
deleteFirstsBy eq = F.foldl (flip (deleteBy eq))

unionBy :: (a -> a -> Bool) -> Concat a -> Concat a -> Concat a
unionBy = undefined

intersectBy :: (a -> a -> Bool) -> Concat a -> Concat a -> Concat a
intersectBy = undefined

groupBy :: (a -> a -> Bool) -> Concat a -> Concat (Concat a)
groupBy = undefined

sortBy :: (a -> a -> Ordering) -> Concat a -> Concat a
sortBy f xs = Whole $ L.sortBy f $ toList xs

-- A helper function so that we know when to stop searching for a place to
-- insert.
insertBy' :: (a -> a -> Ordering) -> a -> Concat a -> (Concat a, Maybe a)
insertBy' _ x ys@(Whole []) = (ys, Just x)
insertBy' cmp x ys@(Whole ys'@(y:ys'')) = case cmp x y of
     GT -> case insertBy' cmp x (Whole ys'') of
         (zs, Nothing) -> (cons y zs, Nothing)
         (_, z) -> (ys, z)
     _  -> (Whole (x : ys'), Nothing)
insertBy' cmp a ws@(Split xs ys) =
    case insertBy' cmp a xs of
        (zs, Nothing) -> (Split zs ys, Nothing)
        _ -> case insertBy' cmp a ys of
            (zs, Nothing) -> (Split xs zs, Nothing)
            (_, Just x) -> (ws, Just x)

insertBy :: (a -> a -> Ordering) -> a -> Concat a -> Concat a
insertBy cmp a (Whole xs) = Whole $ L.insertBy cmp a xs
insertBy cmp a xs = case insertBy' cmp a xs of
    (zs, Nothing) -> zs
    (zs, Just x)  -> snoc zs x

maximumBy :: (a -> a -> Ordering) -> Concat a -> a
maximumBy = F.maximumBy

minimumBy :: (a -> a -> Ordering) -> Concat a -> a
minimumBy = F.minimumBy

genericLength :: Num i => Concat b -> i
genericLength = undefined

genericTake :: Integral i => i -> Concat a -> Concat a
genericTake = undefined

genericDrop :: Integral i => i -> Concat a -> Concat a
genericDrop = undefined

genericSplitAt :: Integral i => i -> Concat b -> (Concat b, Concat b)
genericSplitAt = undefined

genericIndex :: Integral a => Concat b -> a -> b
genericIndex = undefined

genericReplicate :: Integral i => i -> a -> Concat a
genericReplicate = undefined
