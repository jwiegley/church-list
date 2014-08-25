{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides a Tree-based lazy list representation which offers
--   O(1) concatenation and `snoc`, similarly to difference lists, but also
--   examine examination of the results.  Since many operations require
--   walking the tree, they are computationally more expensive than regular
--   lists, but are ideal for the situation where you must constantly append
--   to, and examine, a list that you are building up.
module Data.Church.List
       ( module Data.Church.List
       , module F
       )
       where

import           Control.Applicative
import           Control.Arrow hiding (loop)
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
import           Prelude (Num, Integral, (-), error)
import           Text.Show

newtype List a = List (forall r. Monoid r => (a -> r) -> r)

instance Show a => Show (List a) where
    show (List c) = show (c (:[]))

instance Functor List where
    fmap f (List c) = List (\k -> c (k . f))

instance Foldable List where
    foldMap f (List c) = c f

instance Traversable List where
    traverse f (List c) =
        appEndo (c (\a -> Endo (liftA2 (<>) (pure <$> f a))))
            (pure mempty)

instance Applicative List where
    pure x = List (\k -> k x)
    List f <*> List x = List (\k -> f (\g -> x (k .g)))

instance Monad List where
    return = pure
    List m >>= f = m f

instance Monoid (List a) where
    mempty  = List $ const mempty
    List x `mappend` List y = List $ \k -> x k `mappend` y k

instance Alternative List where
    empty = mempty
    (<|>) = mappend

instance MonadPlus List where
    mzero = mempty
    mplus = mappend

-- -- | Case analyze a concat type's head and tail.
uncons :: List a -> Maybe (a, List a)
uncons c = (, tail c) <$> headMay c

-- | Case analyze a concat type's head and tail.
caseList :: b -> (a -> List a -> b) -> List a -> b
caseList b f xs = maybe b (uncurry f) (uncons xs)

cons :: a -> List a -> List a
cons x = (pure x <>)

snoc :: List a -> a -> List a
snoc c x = c <> pure x

fromList :: [a] -> List a
fromList xs = List $ \k -> F.foldMap k xs

head :: List a -> a
head = fromMaybe (error "Data.List.List.head: empty list") . headMay

headMay :: List a -> Maybe a
headMay (List c) = getFirst $ c (First . Just)

init :: List a -> List a
init = fromList . L.init . F.toList

initMay :: List a -> Maybe (List a)
initMay c = init c <$ headMay c

tail :: List a -> List a
tail = fromList . L.tail . F.toList

tailMay :: List a -> Maybe (List a)
tailMay c = tail c <$ headMay c

last :: List a -> a
last = fromMaybe (error "Data.List.List.last: empty list") . lastMay

lastMay :: List a -> Maybe a
lastMay (List c) = getLast $ c (Last . Just)

null :: List a -> Bool
null = isNothing . headMay

length :: List a -> Int
length (List c) = getSum $ c (const (Sum 1))

map :: (a -> b) -> List a -> List b
map = fmap

reverse :: List a -> List a
reverse = fromList . L.reverse . F.toList

intersperse :: a -> List a -> List a
intersperse a = fromList . L.intersperse a . F.toList

intercalate :: List a -> List (List a) -> List a
intercalate xs xss = concat (intersperse xs xss)

transpose :: List (List a) -> List (List a)
transpose =
    fromList . L.map fromList . L.transpose . L.map F.toList . F.toList

subsequences :: List a -> List (List a)
subsequences = fromList . L.map fromList . L.subsequences . F.toList

permutations :: List a -> List (List a)
permutations = fromList . L.map fromList . L.permutations . F.toList

concat :: List (List a) -> List a
concat = (>>= id)

concatMap :: (a -> List b) -> List a -> List b
concatMap = (=<<)

scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f q = fromList . L.scanl f q . F.toList

scanl1 :: (a -> a -> a) -> List a -> List a
scanl1 f = caseList mempty (scanl f)

scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f q = fromList . L.scanr f q . F.toList

scanr1 :: (a -> a -> a) -> List a -> List a
scanr1 f = caseList mempty (scanr f)

mapAccumL :: (acc -> x -> (acc, y)) -> acc -> List x -> (acc, List y)
mapAccumL f q = fmap fromList . L.mapAccumL f q . F.toList

mapAccumR :: (acc -> x -> (acc, y)) -> acc -> List x -> (acc, List y)
mapAccumR f q = fmap fromList . L.mapAccumR f q . F.toList

iterate :: (a -> a) -> a -> List a
iterate f a = List $ \k -> fix (\loop b -> k b <> loop (f b)) a

repeat :: a -> List a
repeat a = List $ fix . (<>) . ($ a)

replicate :: Int -> a -> List a
replicate n a
    | n <= 0 = mempty
    | otherwise = List $ \k ->
        fix (\loop i -> if i == 0
                        then mempty
                        else k a <> loop (i-1)) n

cycle :: List a -> List a
cycle xs = xs <> cycle xs

unfoldr :: (b -> Maybe (a, b)) -> b -> List a
unfoldr f b = List $ \k ->
    fix (\loop c -> case f c of
              Nothing -> mempty
              Just (a, b') -> k a <> loop b') b

splitAt :: Int -> List a -> (List a, List a)
splitAt n = (fromList *** fromList) . L.splitAt n . F.toList

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile f = fromList . L.takeWhile f . F.toList

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile f = fromList . L.dropWhile f . F.toList

dropWhileEnd :: (a -> Bool) -> List a -> List a
dropWhileEnd p = fromList . L.dropWhileEnd p . F.toList

span :: (a -> Bool) -> List a -> (List a, List a)
span f = (fromList *** fromList) . L.span f . F.toList

break :: (a -> Bool) -> List a -> (List a, List a)
break f = (fromList *** fromList) . L.break f . F.toList

stripPrefix :: Eq a => List a -> List a -> Maybe (List a)
stripPrefix x y = fromList <$> L.stripPrefix (F.toList x) (F.toList y)

group :: Eq a => List a -> List (List a)
group = fromList . fmap fromList . L.group . F.toList

inits :: List a -> List (List a)
inits = fromList . fmap fromList . L.inits . F.toList

tails :: List a -> List (List a)
tails = fromList . fmap fromList . L.tails . F.toList

isPrefixOf :: Eq a => List a -> List a -> Bool
isPrefixOf x y = F.toList x `L.isPrefixOf` F.toList y

isSuffixOf :: Eq a => List a -> List a -> Bool
isSuffixOf x y = F.toList x `L.isSuffixOf` F.toList y

isInfixOf :: Eq a => List a -> List a -> Bool
isInfixOf x y = F.toList x `L.isInfixOf` F.toList y

lookup :: Eq a => a -> List (a, b) -> Maybe b
lookup a (List c) = getFirst $ c $ \(x, b) ->
    First (mfilter (const (a == x)) (Just b))

filter :: (a -> Bool) -> List a -> List a
filter f (List c) = c $ \a -> mfilter f (pure a)

partition :: (a -> Bool) -> List a -> (List a, List a)
partition f (List c) = c $ \a ->
    if f a then (pure a, mempty) else (mempty, pure a)

(!!) :: List a -> Int -> a
xs !! n = F.toList xs L.!! n

elemIndex :: Eq a => a -> List a -> Maybe Int
elemIndex x = findIndex (x ==)

elemIndices :: Eq a => a -> List a -> List Int
elemIndices x = findIndices (x ==)

findIndex :: (a -> Bool) -> List a -> Maybe Int
findIndex p = listToMaybe . F.toList . findIndices p

findIndices :: (a -> Bool) -> List a -> List Int
findIndices f = fromList . L.findIndices f . F.toList

zip :: List a -> List b -> List (a, b)
zip = zipWith (,)

zip3 :: List a -> List b -> List c -> List (a, b, c)
zip3 = zipWith3 (,,)

zip4 :: List a -> List b -> List c -> List d -> List (a, b, c, d)
zip4 = zipWith4 (,,,)

zip5 :: List a -> List b -> List c -> List d -> List e
     -> List (a, b, c, d, e)
zip5 = zipWith5 (,,,,)

zip6 :: List a -> List b -> List c -> List d -> List e -> List f
     -> List (a, b, c, d, e, f)
zip6 = zipWith6 (,,,,,)

zip7 :: List a
     -> List b
     -> List c
     -> List d
     -> List e
     -> List f
     -> List g
     -> List (a, b, c, d, e, f, g)
zip7 = zipWith7 (,,,,,,)

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith z
    (uncons -> Just (a, as))
    (uncons -> Just (b, bs))
    = z a b `cons` zipWith z as bs
zipWith _ _ _ = mempty

zipWith3 :: (a -> b -> c -> d) -> List a -> List b -> List c -> List d
zipWith3 z
    (uncons -> Just (a, as))
    (uncons -> Just (b, bs))
    (uncons -> Just (c, cs))
    = z a b c `cons` zipWith3 z as bs cs
zipWith3 _ _ _ _ = mempty

zipWith4 :: (a -> b -> c -> d -> e)
         -> List a
         -> List b
         -> List c
         -> List d
         -> List e
zipWith4 z
    (uncons -> Just (a, as))
    (uncons -> Just (b, bs))
    (uncons -> Just (c, cs))
    (uncons -> Just (d, ds))
    = z a b c d `cons` zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _ = mempty

zipWith5 :: (a -> b -> c -> d -> e -> f)
         -> List a
         -> List b
         -> List c
         -> List d
         -> List e
         -> List f
zipWith5 z
    (uncons -> Just (a, as))
    (uncons -> Just (b, bs))
    (uncons -> Just (c, cs))
    (uncons -> Just (d, ds))
    (uncons -> Just (e, es))
    = z a b c d e `cons` zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _ = mempty

zipWith6 :: (a -> b -> c -> d -> e -> f -> g)
         -> List a
         -> List b
         -> List c
         -> List d
         -> List e
         -> List f
         -> List g
zipWith6 z
    (uncons -> Just (a, as))
    (uncons -> Just (b, bs))
    (uncons -> Just (c, cs))
    (uncons -> Just (d, ds))
    (uncons -> Just (e, es))
    (uncons -> Just (f, fs))
    = z a b c d e f `cons` zipWith6 z as bs cs ds es fs
zipWith6 _ _ _ _ _ _ _ = mempty

zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h)
         -> List a
         -> List b
         -> List c
         -> List d
         -> List e
         -> List f
         -> List g
         -> List h
zipWith7 z
    (uncons -> Just (a, as))
    (uncons -> Just (b, bs))
    (uncons -> Just (c, cs))
    (uncons -> Just (d, ds))
    (uncons -> Just (e, es))
    (uncons -> Just (f, fs))
    (uncons -> Just (g, gs))
    = z a b c d e f g `cons` zipWith7 z as bs cs ds es fs gs
zipWith7 _ _ _ _ _ _ _ _ = mempty

unzip :: List (a, b) -> (List a, List b)
unzip = F.foldr
             (\(a,b) ~(as,bs) ->
               (a `cons` as,
                b `cons` bs))
             (mempty,mempty)

unzip3 :: List (a, b, c) -> (List a, List b, List c)
unzip3 = F.foldr
             (\(a,b,c) ~(as,bs,cs) ->
               (a `cons` as,
                b `cons` bs,
                c `cons` cs))
             (mempty,mempty,mempty)

unzip4 :: List (a, b, c, d) -> (List a, List b, List c, List d)
unzip4 = F.foldr
             (\(a,b,c,d) ~(as,bs,cs,ds) ->
               (a `cons` as,
                b `cons` bs,
                c `cons` cs,
                d `cons` ds))
             (mempty,mempty,mempty,mempty)

unzip5 :: List (a, b, c, d, e)
       -> (List a, List b, List c, List d, List e)
unzip5 = F.foldr
             (\(a,b,c,d,e) ~(as,bs,cs,ds,es) ->
               (a `cons` as,
                b `cons` bs,
                c `cons` cs,
                d `cons` ds,
                e `cons` es))
             (mempty,mempty,mempty,mempty,mempty)

unzip6 :: List (a, b, c, d, e, f)
       -> (List a, List b, List c, List d, List e, List f)
unzip6 = F.foldr
             (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) ->
               (a `cons` as,
                b `cons` bs,
                c `cons` cs,
                d `cons` ds,
                e `cons` es,
                f `cons` fs))
             (mempty,mempty,mempty,mempty,mempty,mempty)

unzip7 :: List (a, b, c, d, e, f, g)
       -> (List a, List b, List c, List d, List e, List f, List g)
unzip7 = F.foldr
             (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) ->
               (a `cons` as,
                b `cons` bs,
                c `cons` cs,
                d `cons` ds,
                e `cons` es,
                f `cons` fs,
                g `cons` gs))
             (mempty,mempty,mempty,mempty,mempty,mempty,mempty)

lines :: String -> List String
lines = fromList . L.lines

words :: String -> List String
words = fromList . L.words

unlines :: List String -> String
unlines = L.unlines . F.toList

unwords :: List String -> String
unwords = L.unwords . F.toList

nub :: Eq a => List a -> List a
nub = fromList . L.nub . F.toList

delete :: Eq a => a -> List a -> List a
delete = deleteBy (==)

(\\) :: Eq a => List a -> List a -> List a
(\\) = F.foldl (flip delete)

union :: Eq a => List a -> List a -> List a
union = unionBy (==)

intersect :: Eq a => List a -> List a -> List a
intersect = intersectBy (==)

sort :: Ord a => List a -> List a
sort = sortBy compare

insert :: Ord a => a -> List a -> List a
insert = insertBy compare

nubBy :: (a -> a -> Bool) -> List a -> List a
nubBy f = fromList . L.nubBy f . F.toList

deleteBy :: (a -> a -> Bool) -> a -> List a -> List a
deleteBy f a = fromList . L.deleteBy f a . F.toList

deleteFirstsBy :: (a -> a -> Bool) -> List a -> List a -> List a
deleteFirstsBy eq = F.foldl (flip (deleteBy eq))

unionBy :: (a -> a -> Bool) -> List a -> List a -> List a
unionBy f x y = fromList $ L.unionBy f (F.toList x) (F.toList y)

intersectBy :: (a -> a -> Bool) -> List a -> List a -> List a
intersectBy f x y = fromList $ L.intersectBy f (F.toList x) (F.toList y)

groupBy :: (a -> a -> Bool) -> List a -> List (List a)
groupBy f = fromList . fmap fromList . L.groupBy f . F.toList

sortBy :: (a -> a -> Ordering) -> List a -> List a
sortBy f = fromList . L.sortBy f . F.toList

insertBy :: (a -> a -> Ordering) -> a -> List a -> List a
insertBy cmp a = fromList . L.insertBy cmp a . F.toList

genericLength :: Num i => List b -> i
genericLength = L.genericLength . F.toList

genericTake :: Integral i => i -> List a -> List a
genericTake n = fromList . L.genericTake n . F.toList

genericDrop :: Integral i => i -> List a -> List a
genericDrop n = fromList . L.genericDrop n . F.toList

genericSplitAt :: Integral i => i -> List b -> (List b, List b)
genericSplitAt n = (fromList *** fromList) . L.genericSplitAt n . F.toList

genericIndex :: Integral a => List b -> a -> b
genericIndex xs = L.genericIndex (F.toList xs)

genericReplicate :: Integral i => i -> a -> List a
genericReplicate n a
    | n <= 0    = mempty
    | otherwise = List $ \k ->
        fix (\loop i -> if i == 0
                        then mempty
                        else k a <> loop (i-1)) n
