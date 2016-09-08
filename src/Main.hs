module Main where
import System.IO.Unsafe (unsafePerformIO)
import Data.Array.IO
import Data.Array.MArray
import Data.Array.IArray
import Control.Monad ((=<<))

n :: Int
n = 100

fibarray :: IOArray Int (Integer, Int)
fibarray = unsafePerformIO $ do
    a <- newArray (1, n) (0, 0)
    writeArray a 1 (1, 0)
    writeArray a 2 (1, 0)
    return a

fib :: Int -> Integer
fib n = unsafePerformIO $ do
    (val, cnt) <- readArray fibarray n
    if val == 0 then
        writeArray fibarray n $ (fib (n-1) + fib (n-2), cnt+1)
    else
        return ()
    fst <$> readArray fibarray n 

main :: IO ()
main = do
    putStrLn . show . fib $ 20
    putStrLn "\n"
    putStrLn . show =<< (freeze fibarray :: IO (Array Int (Integer, Int)))
    putStrLn "\n\n"
    putStrLn . show . fib $ 70
    putStrLn "\n"
    putStrLn . show =<< (freeze fibarray :: IO (Array Int (Integer, Int)))
    putStrLn "\n\n"
    putStrLn . show . fib $ 100
    putStrLn "\n"
    putStrLn . show =<< (freeze fibarray :: IO (Array Int (Integer, Int)))