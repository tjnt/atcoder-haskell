{-# OPTIONS_GHC -fno-warn-unused-binds
                -fno-warn-unused-matches
                -fno-warn-type-defaults
#-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Memo {{{1
-- ghciで実行時間の出力
-- :set +s

module Snippet where

-- import modules {{{1
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Array.IArray
import           Data.Array.IO
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Bits
import qualified Data.ByteString.Char8 as BS
import           Data.Int              (Int64)
import           Data.List
import           Data.Maybe            (fromJust)

-- 入力処理 {{{1
inputExample :: IO ()
inputExample = do
    -- 数値
    n <- readLn :: IO Int
    -- 数値リスト
    _<- map read . words <$> getLine :: IO [Int]
    -- 複数行の数値
    _ <- map read . lines <$> getContents :: IO [Int]
    -- 複数行の数値リスト
    _ <- map (map read . words) . lines <$> getContents :: IO [[Int]]
    -- 複数行の数値リストをタプルに変換
    _ <- map ((\[a,b] -> (a,b)) . (map read . words)) . lines
         <$> getContents :: IO [(Int,Int)]

    -- 複数行の数値（Control.Monad）
    _ <- replicateM n readLn :: IO [Int]
    -- 複数行の数値リスト（Control.Monad）
    _ <- replicateM n $ map read . words <$> getLine :: IO [[Int]]

    -- 高速な入力処理
    -- import qualified Data.ByteString.Char8 as BS
    -- import Data.Maybe
    -- 数値リスト
    _ <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
    -- 複数行の数値
    _ <- map (fst . fromJust . BS.readInt) . BS.lines <$> BS.getContents
    -- 複数行の数値リスト
    _ <- map (map (fst . fromJust . BS.readInt) . BS.words) . BS.lines
         <$> BS.getContents
    -- 複数行の数値リストをタプルに変換
    _ <- map ((\[a,b] -> (a,b)) . map (fst . fromJust . BS.readInt) . BS.words)
         . BS.lines <$> BS.getContents
    -- 複数行の少数値リストをタプルに変換
    _ <- map ((\[a,b] -> (a,b)) . map ((read :: String -> Double) . BS.unpack) . BS.words)
        . BS.lines <$> BS.getContents :: IO [(Double,Double)]

    -- 迷路などの格子状データの読み出し
    [h,w] <- map read . words <$> getLine :: IO [Int]
    _ <- listArray ((0,0),(h-1,w-1))
         . concatMap BS.unpack . BS.lines
         <$> BS.getContents :: IO (Array (Int,Int) Char)

    return ()

-- 型変換 {{{1
convertingExample :: IO ()
convertingExample = do
    let i1 = 1 :: Int
    -- Int -> Double
    let d1 = fromIntegral i1 :: Double
    -- Double -> Int (切り上げ)
    let _ = ceiling d1 :: Int
    -- Double -> Int (切り捨て)
    let _ = floor d1 :: Int
    -- Double -> Int (0に近い方向へ切り捨て)
    let _ = truncate d1 :: Int
    -- Double -> Int (丸め)
    let _ = round d1 :: Int
    return ()

-- Arrayのサンプル {{{1
arrayExample :: IO ()
arrayExample = do
    -- import Data.Array.IArray
    let li = "abcdefg"
        n  = length li
    -- 配列の生成
    let a = listArray (0, n-1) li :: Array Int Char
    -- インデックスアクセス
    putChar $ a!0
    putChar $ a!1
    -- 配列の変更
    let a' = a // [(0,'A'), (1,'B')]
    -- リスト変換
    print $ elems a'

    -- import Data.Array.IO
    -- 配列の生成
    ma <- newListArray (0, n-1) li :: IO (IOUArray Int Char)
    -- インデックスアクセス
    putChar =<< readArray ma 1
    putChar =<< readArray ma 2
    -- 配列の変更
    writeArray ma 1 'B'
    writeArray ma 2 'C'
    -- リスト変換
    print =<< getElems ma

-- 状態系モナド使用例 {{{1

-- Reader Monad {{{2
readerMonadExample :: IO ()
readerMonadExample =
    -- runReader 関数 状態 -> 結果
    print $ runReader run (1,2)
  where
    -- 型は Reader 状態 返却値
    run :: Reader (Int,Int) [Int]
    run = do
        -- ask  状態の読み出し
        (a,b) <- ask
        -- asks  関数を適用して状態を読み出し
        c <- asks fst
        -- local  一時的に変更を加えた状態を扱う
        (d,e) <- local (\(i,j) -> (i*2, j*2)) $ do
            a' <- asks fst
            b' <- asks snd
            return (a',b')
        -- 結果の返却
        return [a,b,c,d,e]

-- Writer Monad {{{2
writerMonadExample :: IO ()
writerMonadExample = do
    -- runWriter 関数 -> (結果,状態)
    print $ runWriter run
    -- 状態のみ取得する場合
    -- execWriter 関数 -> 状態
    print $ execWriter run
  where
    -- 型は Writer 状態 返却値
    run :: Writer String Int
    run = do
        -- tell  状態の追記
        tell "a"
        tell "b"
        -- 結果の返却
        return 1

-- State Monad {{{2
stateMonadExample :: IO ()
stateMonadExample = do
    -- runState 関数 初期状態 -> (結果,状態)
    print $ runState run "a"
    -- 状態のみ取得する場合
    -- execState 関数 初期状態 -> 状態
    print $ execState run "a"
    -- 結果のみ取得する場合
    print $ evalState run "a"
  where
    -- 型は State 状態 返却値
    run :: State String Int
    run = do
        -- get  状態の取得
        a <- get
        -- put  状態の設定
        put "b"
        -- modify  状態の書き換え
        modify (++a)
        -- 結果の返却
        return 1

-- RWS Monad {{{2
-- Reader + Writer + State で RWS
rwsMonadExample :: IO ()
rwsMonadExample = do
    -- runRWS 関数 状態(R) 状態(S) -> (結果,状態(S),状態(W))
    print $ runRWS run "r1" "s1"
    -- 結果と状態(W)のみ取得する場合
    -- evalRWS 関数 状態(R) 状態(S) -> (結果,状態(W))
    print $ evalRWS run "r1" "s1"
    -- 状態(S)と状態(W)のみ取得する場合
    -- execRWS 関数 状態(R) 状態(S) -> (状態(S),状態(W))
    print $ execRWS run "r1" "s1"
  where
    -- 型は RWS 状態(R) 状態(W) 状態(S) 返却値
    run :: RWS String String String Int
    run = do
        -- 状態へのアクセスはReader/Writer/Stateと同じ
        rs <- ask
        tell rs
        ss <- get
        tell ss
        put "s2"
        return 1

-- その他の例 {{{1
otherExample :: IO ()
otherExample = do
    -- GCD (最大公約数)
    let _ = gcd 12 18
    -- LCM (最小公倍数)
    let _ = lcm 2 3
    -- 順列
    -- import Data.List
    let _ = permutations [1,2,3]
    -- 組み合わせの生成
    -- ex: 0 or 1を3回選ぶ組み合わせのパターンを生成する
    let _ = replicateM 3 [0,1]
    -- n個の中から重複する組み合わせの数
    let n = 255
    let _ = n * (n - 1) `div` 2
    return ()

-- リスト操作 {{{1

-- 組み合わせ {{{2
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs' ]

-- 旧バージョン
-- combinations :: Int -> [a] -> [[a]]
-- combinations 0 _      = [[]]
-- combinations _ []     = []
-- combinations n (x:xs) = [x:y | y <- combinations (n - 1) xs] ++ combinations n xs

-- zipWith(正格評価) {{{2
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = x `seq` x : zipWith' f as bs
  where
    x = f a b
zipWith' _ _ _ = []

-- 2つずつ処理するmap {{{2
map2 :: (a -> a -> b) -> [a] -> [b]
map2 _ []       = []
map2 _ [_]      = []
map2 f (i:j:xs) = f i j : map2 f xs

-- 2つずつ処理するtakeWhile {{{2
takeWhile2 :: (a -> a -> Bool) -> [a] -> [a]
takeWhile2 _ []  = []
takeWhile2 _ [x] = [x]
takeWhile2 p (i:j:xs) =
    if i `p` j then i : takeWhile2 p (j:xs) else [i]

-- 2つずつ処理するdropWhile {{{2
dropWhile2 :: (a -> a -> Bool) -> [a] -> [a]
dropWhile2 _ []  = []
dropWhile2 _ [i] = [i]
dropWhile2 p (i:j:xs) =
    if i `p` j then dropWhile2 p (j:xs) else i:j:xs

-- 要素を交互に分割 {{{2
alternateSplit :: [a] -> ([a],[a])
alternateSplit = foldr (\x (l,r)-> (x:r,l)) ([],[])

-- 要素を交互に結合 {{{2
alternateJoin :: [a] -> [a] -> [a]
alternateJoin [] _      = []
alternateJoin (x:xs) ys = x : alternateJoin ys xs

-- リストを任意の値で分割 {{{2
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn t xs =
    case break (==t) xs of
        (a,[])  -> [a]
        (a,[_]) -> a:[[]]
        (a,_:b) -> a : splitOn t b

-- リストを任意のいずれかの値で分割 {{{2
splitOneOf :: Eq a => [a] -> [a] -> [[a]]
splitOneOf _ [] = []
splitOneOf ts xs =
    case break (`elem` ts) xs of
        (a,[])  -> [a]
        (a,[_]) -> a:[[]]
        (a,_:b) -> a : splitOneOf ts b

-- リストを与えられた条件に合致する要素で分割 {{{2
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs =
    case break p xs of
        (a,[])  -> [a]
        (a,[_]) -> a:[[]]
        (a,_:b) -> a : splitWhen p b

-- リストを任意個数に分割 {{{2
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (a,b) = splitAt n xs in a : chunksOf n b

-- リストを指定したnおよびm入力仕様に従い、一連のサブリストに分割 {{{2
-- divvy 5 5 [1..20] == [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20]]
-- divvy 5 2 [1..10] == [[1,2,3,4,5],[3,4,5,6,7],[5,6,7,8,9]]
divvy :: Int -> Int -> [a] -> [[a]]
divvy n m xs
  | length xs < n = []
  | otherwise = let (a,b) = splitAt n xs
                 in a : divvy n m (drop m xs)

-- 部分列の切り出し {{{2
-- part (start index) (length)
-- part 2 3 "abcdefg"
-- cde
part :: Int -> Int -> [a] -> [a]
part i n = take n . drop i

-- 部分列の数を数える {{{2
-- countPartOf "ab" "abcababcd"
-- 3
countPartOf :: Eq a => [a] -> [a] -> Int
countPartOf _ [] = 0
countPartOf [] _ = 0
countPartOf a b = if a `isPrefixOf` b
                  then succ $ countPartOf a (drop (length a) b)
                  else countPartOf a (drop 1 b)

-- 部分列の生成 {{{2
-- genPart 3 [1..5]
-- [[1,2,3],[2,3,4],[3,4,5]]
genPart :: Int -> [a] -> [[a]]
genPart n a = [(take n . drop i) a | i <- [0..length a - n]]

-- リストの指定された位置の要素を変更 {{{2
modifyIndexOf :: Int -> a -> [a] -> [a]
modifyIndexOf i v =
    map (\(j,x) -> if i == j then v else x) . zip [0..]

-- リストの指定された位置の要素を削除 {{{2
deleteIndexOf :: Int -> [a] -> [a]
deleteIndexOf i = reverse . foldl (\a (j,x) -> if i == j then a else x:a) [] . zip [0..]

-- 数値のリストを結合
concatIntList :: Integral a => [a] -> a
concatIntList xs = go (length xs - 1) xs 0
  where
    go _ [] sum     = sum
    go n (x:xs) sum = go (n-1) xs (sum + x * 10^n)

-- 探索 {{{1
-- dfs 深さ優先探索
-- dfs :: (a -> [a]) -> a -> [a]
-- dfs next = dfs'
--   where
--     dfs' n = n : concatMap dfs' (next n)

-- bfs 幅優先探索
-- bfs :: (a -> [a]) -> a -> [a]
-- bfs next node = bfs' [node]
--   where
--     bfs' xs = xs ++ bfs' (concatMap next xs)

-- ２分探索 {{{1
--
binarySearch :: (Integral i, Ix i, Ord e, IArray a e) => e -> a i e -> Maybe i
binarySearch x a =
    let (b,e) = bounds a in bsearch b (e + 1)
  where
    bsearch b e
      | e == b    = Nothing
      | x == a!p  = Just p
      | x < a!p   = bsearch b p
      | otherwise = bsearch (p + 1) e
      where p = (b + e) `div` 2

lowerBound:: (Integral i, Ix i, Ord e, IArray a e) => e -> a i e -> i
lowerBound x a =
    let (b,e) = bounds a in bsearch b (e + 1)
  where
    bsearch b e
      | e == b    = b
      | x <= a!p  = bsearch b p
      | otherwise = bsearch (p + 1) e
      where p = (b + e) `div` 2

upperBound:: (Integral i, Ix i, Ord e, IArray a e) => e -> a i e -> i
upperBound x a =
    let (b,e) = bounds a in bsearch b (e + 1)
  where
    bsearch b e
      | e == b    = b
      | x < a!p   = bsearch b p
      | otherwise = bsearch (p + 1) e
      where p = (b + e) `div` 2

-- ２分法
--   f     判定条件
--   (l,h) 探索範囲
bisectionMethod :: Integral a => (a -> Bool) -> (a,a) -> a
bisectionMethod f (l,h)
  | h - l == 1 = l
  | f m        = bisectionMethod f (m,h)
  | otherwise  = bisectionMethod f (l,m)
  where
    m = l + (h-l) `div` 2

-- モジュラー計算 {{{1
-- import Data.Int (Int64)
modulus :: Int64
modulus = 1000000007

addMod, subMod, mulMod :: Int64 -> Int64 -> Int64
addMod x y
  | x + y >= modulus = x + y - modulus
  | otherwise        = x + y

subMod x y
  | x - y < modulus = x - y + modulus
  | otherwise       = x - y

mulMod x y = (x * y) `rem` modulus

-- 切り上げ割り算 {{{1
ceilDiv :: Integral a => a -> a -> a
ceilDiv t s = (t + s - 1) `div` s

-- 実数の整数判定 {{{1
isInteger :: RealFrac a => a -> Bool
isInteger x = x == fromInteger (round x)

-- 階乗 {{{1
fact :: Integral a => a -> a
fact n = product [1..n]

-- fact :: Integral a => a -> a
-- fact 0 = 1
-- fact n = n * fact (n - 1)

-- 末尾再帰 + 正格評価 (速い)
fact' :: Integral a => a -> a
fact' n = f n 1
  where
    f 0 b  = b
    f a !b = f (a - 1) (a * b)

-- 繰り返し二乗法 {{{1
--   nのp乗をmで割った余り
powMod :: Integral a => a -> a -> a -> a
powMod n p m
  | p == 0    = 1
  | odd p     = n * powMod n (p-1) m `mod` m
  | otherwise = let t = powMod n (p `div` 2) m
                 in (t^2) `mod` m

-- 素数生成 {{{1
-- primes :: Integral a => a -> [a]
-- primes n = sieve [2..n]
--   where
--     sieve []     = []
--     sieve (x:xs) = x : sieve [y | y <- xs, y `rem` x /= 0]

-- （速い）
primes :: Integral a => a -> [a]
primes n = 2 : f [3] [3,5..n]
  where
    f [] _ = []
    f (x:xs) ys =
        let (ps, qs) = span (< x^2) ys
         in ps ++ f (xs ++ ps) [z | z <- qs, z `rem` x /= 0]

-- 素数判定
isPrime :: Integral a => a -> Bool
isPrime n
  | n <= 2    = n == 2
  | otherwise = odd n && f 3
  where
    f i
      | i^2 > n   = True
      | otherwise = n `rem` i /= 0 && f (i+2)

-- 素数判定用配列 {{{1
primesArray :: Int -> Array Int Bool
primesArray n = listArray (0,n) $ map isPrime [0..n]

-- エラトステネスの篩 {{{1
-- import Data.Array.Unboxed
-- import Data.Array.ST
sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
    a <- newArray (0,n) True
    mapM_ (uncurry (writeArray a))
        [ (0,False), (1,False), (2,True) ]
    mapM_ (flip (writeArray a) False) [4,6..n]
    forM_ [3,5..n] $ \i -> do
        b <- readArray a i
        when b
          $ mapM_ (flip (writeArray a) False) [2*i, 3*i..n]
    return a

-- test1 n = let s = sieve n in [ i | i <- [1..n], s!i ]
-- test2 n = let s = primesArray n in [ i | i <- [1..n], s!i ]

-- 約数列挙 {{{1
divisor :: Integral a => a -> [a]
divisor n = foldr f [] $ takeWhile ((<= n) . (^2)) [1..n]
  where
    f x xs
        | r == 0, q /= x = x : q : xs
        | r == 0         = x : xs
        | otherwise      = xs
      where
        (q, r) = n `divMod` x

-- 順列の数 {{{1
nPr :: Integral a => a -> a -> a
nPr 0 _ = 0
nPr _ 0 = 0
nPr n r = product [(n - r + 1)..n]
-- nPr n r = fact n `div` fact (n - r)

-- 組み合わせの数 {{{1
nCr :: Integral a => a -> a -> a
nCr 0 _ = 0
nCr _ 0 = 0
nCr n r = nPr n r `div` product [1..r]
-- nCr n r = nPr n r `div` fact r

-- マンハッタン距離 {{{1
l1norm :: (Integral a, Floating b) => [a] -> [a] -> b
l1norm a b = fromIntegral . sum $ zipWith (\i j -> abs (i - j)) a b

-- ユークリッド距離 {{{1
l2norm :: (Integral a, Floating b) => [a] -> [a] -> b
l2norm a b = sqrt . fromIntegral . sum $ zipWith (\i j -> (i - j) ^ 2) a b

-- 桁数取得 {{{1
digitNum :: Integral a => a -> a
digitNum = go 0
  where
    go i 0 = i
    go i x = go (i+1) (x `div` 10)

-- 桁数取得 (対数版) {{{1
--   10^38くらいまでいける
digitNumLogB :: Integral a => a -> a -> a
digitNumLogB _ 0 = 0
digitNumLogB b n = floor (logBase b' n' + 1)
  where
    b' = fromIntegral b :: Float
    n' = fromIntegral n :: Float

-- 数字和 {{{1
digitSum :: Integral a => a -> a
digitSum 0 = 0
digitSum n = n `mod` 10 + digitSum (n `div` 10)

-- 数字和（引数が文字列） {{{1
digitSumS :: String -> Int
digitSumS s = sum [read [c] | c <- show s]

-- nの倍数までの差 {{{1
-- ex:
--   multipleDiff 10 24 => 6
--   multipleDiff 10 40 => 0
multipleDiff :: Integral a => a -> a -> a
multipleDiff n x = (n - x `mod` n) `mod` n

-- nで割り切れる回数を算出 {{{1
-- ex:
--   divableN 2 8 => 3
--   divableN 3 9 => 2
divableN :: Integral a => a -> a -> a
divableN n x
  | x `rem` n /= 0 = 0
  | otherwise      = 1 + divableN n (x `div` n)

-- ビット演算 {{1

-- 1をXbitシフトする
shift1 :: Int -> Int
shift1 = shift 1

-- 条件を満たした場合、指定位置のbitを立てる
bitOn :: Bool -> Int -> Int -> Int
bitOn c i x = if c then x .|. shift1 i else x

-- Bool値のリストをビット列に変換
conditionsBit :: [Bool] -> Int
conditionsBit xs =
    foldl' (\a (i,c) -> bitOn c i a) 0 $ zip [0..] xs

-- Heap {{{1
--
type HeapPolicy a = (a -> a -> Bool)

data Tree a = Empty
            | Fork Int a (Tree a) (Tree a)

data Heap a = Heap (HeapPolicy a) (Tree a)

empty :: HeapPolicy a -> Heap a
empty = flip Heap Empty

rank :: Tree a -> Int
rank Empty          = 0
rank (Fork r _ _ _) = r

joinHeap :: a -> Tree a -> Tree a -> Tree a
joinHeap x a b
  | rank a >= rank b = Fork (rank b + 1) x a b
  | otherwise        = Fork (rank a + 1) x b a

merge :: HeapPolicy a -> Tree a -> Tree a -> Tree a
merge _ t Empty = t
merge _ Empty t = t
merge p h1@(Fork _ x l1 r1) h2@(Fork _ y l2 r2)
  | p x y     = joinHeap x l1 (merge p r1 h2)
  | otherwise = joinHeap y l2 (merge p r2 h1)

push :: a -> Heap a -> Heap a
push x (Heap p t) = Heap p (merge p (Fork 1 x Empty Empty) t)

pop :: Heap a -> Heap a
pop (Heap _ Empty)          = error "empty heap!"
pop (Heap p (Fork _ _ a b)) = Heap p (merge p a b)

top :: Heap a -> a
top (Heap _ Empty)           = error "empty heap!"
top (Heap _ (Fork  _ x _ _)) = x

fromList :: HeapPolicy a -> [a] -> Heap a
fromList p = foldr push (empty p)

toList :: Heap a -> [a]
toList (Heap _ Empty) = []
toList h              = top h : toList (pop h)

isEmpty :: Heap a -> Bool
isEmpty (Heap _ Empty) = True
isEmpty _              = False

-- グラフ関連 {{{1
--
type Vertex a  = a
type Graph a   = Array (Vertex a) [Vertex a]
-- type Graph a   = Array (Vertex a) (S.Set (Vertex a))
type Bounds a  = (Vertex a, Vertex a)
type Edge a    = (Vertex a, Vertex a)
type Table i a = Array (Vertex i) a

-- 有向グラフ {{{2
graph :: (Ix a) => Bounds a -> [Edge a] -> Graph a
graph (b,e) = accumArray (flip (:)) [] (b,e)

-- graph :: (Ix a) => Bounds a -> [Edge a] -> Graph a
-- graph (b,e) = accumArray (flip S.insert) S.empty (b,e)

-- 無向グラフ {{{2
graphU :: (Ix a) => Bounds a -> [Edge a] -> Graph a
graphU bs lst = graph bs $ nub (lst ++ map swap lst)
  where swap (a,b) = (b,a)

-- DFSによる全経路探索 {{{2
--   dfsA "開始点" "グラフ" "経路リスト"
dfsA :: (Ix a) => Vertex a -> Graph a -> [[Vertex a]]
dfsA p g = concatMap (next [p]) (g!p)
  where
    next v x = let v' = (x:v)
                   xs = filter (`notElem` v') (g!x)
                in case xs of
                    [] -> [reverse v']
                    _  -> concatMap (next v') xs


-- 隣接リストから隣接行列への変換
adjL2adjM :: (Int,Int) -> [(Int,[Int])] -> Array (Int,Int) Int
adjL2adjM (b,e) xs = accumArray (flip const) 0 ((b,b),(e,e))
                   $ concatMap f xs
  where f (i,vs) = map (\j -> ((i,j),1)) vs

-- old ver

-- adjacencyList :: (Ix a) => a -> a -> [(a,a)] -> Array a [a]
-- adjacencyList bgn end = accumArray (flip (:)) [] (bgn,end)

-- adjacencyList :: (Ix a, Enum a) => a -> a -> [(a,a)] -> Array a [a]
-- adjacencyList bgn end lst = listArray (bgn,end) $ map (\i ->
--                         M.findWithDefault [] i (aggregate lst)) [bgn..end]
--   where
--     aggregate []           = M.empty
--     aggregate ((x1,x2):xs) = let iw = M.insertWith (\[a] b -> a:b)
--                               in iw x1 [x2] . iw x2 [x1] $ aggregate xs

-- mapFromList :: Ord k => [(k,v)] -> M.Map k [v]
-- mapFromList []         = M.empty
-- mapFromList ((k,v):xs) = M.insertWith (\[a] b -> a:b) k [v] $ mapFromList xs

-- edgesFromList :: Ord a => [(a,a)] -> [(a, a, [a])]
-- edgesFromList xs = map (\(a,b) -> (a,a,b)) . M.toList $ mapFromList xs

-- {{{1
-- vim:set foldmethod=marker:
