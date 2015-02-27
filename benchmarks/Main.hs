module Main where

import Criterion
import Criterion.Main (defaultMain)

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Core.Types
import Graphics.Rendering.Cairo

import qualified DragonCurve as DragonCurve
import qualified Poster as Poster
import qualified Rotations as Rotations

nullCairo :: Diagram Cairo R2 -> IO ()
nullCairo d = withImageSurface FormatARGB32 400 400 $ \surf -> do
    renderWith surf . snd $ renderDia Cairo
        (CairoOptions "foo.png" (Dims 400 400) RenderOnly False) d


main :: IO ()
main = defaultMain
    [ bgroup "diagrams"
        [ bench "DragonCurve" $ whnfIO $ nullCairo DragonCurve.benchDiagram
        , bench "Poster" $ whnfIO $ nullCairo Poster.benchDiagram
        , bench "Rotations" $ whnfIO $ nullCairo Rotations.benchDiagram
        ]
    ]
