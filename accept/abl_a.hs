main :: IO ()
main = readLn >>= putStrLn . concat . (flip replicate) "ACL"
