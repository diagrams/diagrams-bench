{-# LANGUAGE NoMonomorphismRestriction #-}

module Rotations (
  main
, benchDiagram
) where

import           Data.List.Split
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

n :: Int
n = 20

-- Ideally, this function would perform well:
squares = iterateN (n*n) (rotateBy (1/(fromIntegral $ n*n))) (square 1)

-- but it doesn't, due to the iterated calls to rotateBy.
-- this alternative has much better performance:
squares' = [square 1 # rotateBy (fromIntegral x/fromIntegral (n*n))
            | x <- [0..n*n]]
-- in order to get good performance out of nested transformations, it's
-- probably necessary to make significant changes to the Transform type.

main = defaultMain benchDiagram

benchDiagram =
    bg white
  . vcat' with {_catMethod = Distrib, _sep = 1.2}
  . map (hcat' with {_catMethod = Distrib, _sep = 1.2})
  . chunksOf n
  $ squares
