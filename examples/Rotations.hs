{-# LANGUAGE NoMonomorphismRestriction #-}

module Rotations (
  main
, benchDiagram
) where

import           Data.List.Split
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

n = 20

squares = iterateN (n*n) (rotateBy (1/(n*n))) (square 1)

main = defaultMain benchDiagram

benchDiagram =
    bg white
  . vcat' with {catMethod = Distrib, sep = 1.2}
  . map (hcat' with {catMethod = Distrib, sep = 1.2})
  . chunksOf n
  $ squares
