import Text.Printf

main :: IO ()
main = do
    n <- readLn :: IO Int
    let (h,n') = n `divMod` (60 * 60)
    let (m,s)  = n' `divMod` 60
    printf "%02d:%02d:%02d" h m s
