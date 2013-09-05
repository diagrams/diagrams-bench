{-# LANGUAGE FlexibleContexts #-}
module DragonCurve (
  main
, benchDiagram
) where

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Vector

data Tok = F | P | M | X | Y deriving (Eq, Show)

rewriteFunction :: Tok -> [Tok]
rewriteFunction X = [X, P, Y, F, P]
rewriteFunction Y = [M, F, X, M, Y]
rewriteFunction t = [t]

gens :: [[Tok]]
gens = iterate (concatMap rewriteFunction) [F, X]

toks2offsets :: [Tok] -> [R2]
toks2offsets xs = [v | (Just v, _) <- scanl f (Nothing, unitX) xs] where
  f (_, dir) F = (Just dir, dir)
  f (_, dir) P = (Nothing, perp dir)
  f (_, dir) M = (Nothing, negate $ perp dir)
  f (_, dir) _ = (Nothing, dir)

genDiagram :: Renderable (Path R2) b => Int -> Diagram b R2
genDiagram = lw 0.5 . strokeLine . lineFromOffsets . toks2offsets . (!!) gens

benchDiagram :: Renderable (Path R2) b => Diagram b R2
benchDiagram = genDiagram 16

main :: IO ()
main = defaultMain $ genDiagram 16
