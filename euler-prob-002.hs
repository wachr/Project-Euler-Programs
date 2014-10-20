-- euler-prob-002.hs
-- author: Ray Wach
-- date: 2014-10-02
-- info: Program to generate the solution to Project Euler problem 2.

import System.Environment

main :: IO ()
main = do
    -- Read in the first command line argument.
    cap <- getArgs >>= (return . read . head)
    -- Print the result.
    print . sum . filter (even) $ fibs cap

fibs :: Int -> [Int]
fibs cap = takeWhile (< cap) fibs_
  where
    fibs_ = zipWith (+) primer (tail primer)
    primer = (0:1:fibs_)
