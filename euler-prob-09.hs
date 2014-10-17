{-# LANGUAGE ScopedTypeVariables #-}

-- euler-prob-09.hs
-- author: Ray Wach
-- date: 2014-10-16
-- info: This is a set of Haskell types and functions to compute a solution to
--   Project Euler problem 9.

import Control.Monad      (liftM, when)
import Data.Maybe         (fromJust, isJust)
import System.Environment (getArgs, getProgName)
import System.Exit        (exitFailure, exitSuccess)

type Triple = (Int, Int, Int)

tSum :: Triple -> Int
tSum (a,b,c) = a + b + c

tProd :: Triple -> Int
tProd (a,b,c) = a * b * c

isPythagorean :: Triple -> Bool
isPythagorean t@(a,b,c) = positive && monotonic && pyth
  where
    positive  = (a > 0) && (b > 0) && (c > 0)
    monotonic = (a < b) && (b < c)
    pyth      = a^2 + b^2 == c^2

euclid' :: Int -> Int -> Maybe Triple
euclid' m n
    | (m>n) &&
      (odd $ m-n) &&
      ((gcd m n)==1) = let a = m^2 - n^2
                           b = 2 * m * n
                           c = m^2 + n^2
                        in Just (a, b, c)
    | otherwise      = Nothing

euclid :: Int -> Int -> Int -> Maybe Triple
euclid k m n = euclid' m n >>= \(a,b,c) -> Just (k*a, k*b, k*c)

findTripleWithSum :: Int -> [Triple]
findTripleWithSum i =
    [fromJust $ euclid (i `div` (tSum . fromJust $ euclid' m n)) m n
        | m <- ns, n <- ns, m > n, odd $ m-n,
          i `mod` (2*m*(m+n)) == 0, isJust $ euclid' m n]
  where
    ns = [1..(i `div` 2 + 1)]

main :: IO ()
main = do
    args <- getArgs
    -- If no command line args, print usage message and exit.
    when (length args == 0) $ do
        progName <- getProgName
        print $ "Usage: " ++ progName ++ " <sum_of_triple>"
        exitFailure
    target <- return . read . head $ args
    product <- return . map tProd $ findTripleWithSum target
    print product
    exitSuccess
