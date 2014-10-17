#!/usr/bin/env runhaskell
import Text.Printf (printf)
main :: IO ()
main = mapM_ putStrLn [ unwords [
       printf "\ESC[%d;%d;%dm%d;%d;%d\ESC[m" w f b w f b
       | b <- [40 .. 47 :: Int] ]
       | f <- [30 .. 37 :: Int]
       , w <- [ 0 ..  1 :: Int] ]
