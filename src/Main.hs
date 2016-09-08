module Main where
import System.IO.Unsafe (unsafePerformIO)
import Data.Array.IO
import Data.Array.MArray
import Data.Array.IArray
import Control.Monad ((=<<))

n :: Int
n = 100

fibarray :: IOArray Int Integer
fibarray = unsafePerformIO $ do
    a <- newArray (1, n) 0
    writeArray a 1 1
    writeArray a 2 1
    return a

fib n = unsafePerformIO $ do
    val <- readArray fibarray n
    if val == 0 then
        writeArray fibarray n $ fib (n-1) + fib (n-2)
    else
        return ()
    readArray fibarray n

main :: IO ()
main = do
    putStrLn . show . fib $ 20
    putStrLn "\n\n"
    putStrLn . show =<< (freeze fibarray :: IO (Array Int Integer))
    putStrLn "\n\n\n\n"
    putStrLn . show . fib $ 70
    putStrLn "\n\n"
    putStrLn . show =<< (freeze fibarray :: IO (Array Int Integer))
    putStrLn "\n\n\n\n"
    putStrLn . show . fib $ 100
    putStrLn "\n\n"
    putStrLn . show =<< (freeze fibarray :: IO (Array Int Integer))