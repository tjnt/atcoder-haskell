import           Control.Monad
import           Data.List
main = do
    n <- readLn :: IO Int
    a <- replicateM n readLn :: IO [Int]
    let m1 = maximum a
        m2 = maximum $ delete m1 a
    mapM_ (\i -> print $ if i == m1 then m2 else m1) a
