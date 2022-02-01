import qualified Data.Set as S

main :: IO ()
main = do
    _ <- getLine
    s <- words <$> getLine :: IO [String]
    putStrLn $ case S.size $ S.fromList s of
        3 -> "Three"
        4 -> "Four"
        _ -> undefined
