import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.List

check1 :: [Integer] -> Bool
check1 (x:xs) = x == 0 && (length . filter (==0)) xs == 0

check2 :: [Integer] -> Bool
check2 [] = True
check2 [_] = True
check2 (x1:x2:xs) = succ x1 == x2 && check2 (x2:xs)

solve :: [Integer] -> Integer
solve [_] = 1
solve (x1:x2:xs) = (x1^x2) * solve (x2:xs)

main :: IO ()
main = do
    _ <- getLine
    d <- map (fst . fromJust . BS.readInteger)
        . BS.words <$> BS.getLine :: IO [Integer]
    let d' = group $ sort d
    print $
        if check1 d && check2 (map head d')
            then solve (map (fromIntegral . length) d') `rem` 998244353
            else 0
