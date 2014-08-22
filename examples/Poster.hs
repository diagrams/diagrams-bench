{-# LANGUAGE NoMonomorphismRestriction #-}

module Poster (
  main
, benchDiagram
) where

import           Data.Char                              (digitToInt)
import           Data.List                              (intercalate, tails)
import           Data.List.Split                        (chunksOf)
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude                       hiding (timesN)
import           Diagrams.TwoD.Factorization

import           Graphics.SVGFonts

import           Math.NumberTheory.Primes.Factorisation

----------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------

text' d s = (stroke $ textSVG' (TextOpts s lin2 INSIDE_H KERN False d d))
          # fc black # lw none

----------------------------------------------------------------------
-- Showing how factorization diagrams are built up recursively
----------------------------------------------------------------------

-- Abstract schematic

parrow l = hrule l # alignR <> triangle 0.5 # rotateBy (-1/4) # fc black # scaleY 0.5

abstract x =
  hsep 1
  [ vsep 0.5
    [ n # mkBox 2
    , text' 1 "n"
    ] # alignB # translateY (-2)
  , parrow 2
  , vsep 0.5
    [ timesN x # mkBox 3
    , text' 1 (show x ++ "×n")
    ] # alignB # translateY (-2)
  ]

timesN p = primeLayout defaultColors p n

n =
  text' 2 "n" # italic
  <>
  ( circle 1
  # dashing [d,d] none
  # lwL 0.03
  # fcA (white `withOpacity` 0.8)
  )
  where
    d = Local (pi/30)

-- Concrete example

build n = vcat' with {_catMethod = Distrib, _sep = 1.2} . map showStep $ ps
  where
    ps = reverse . tails
       . concatMap (uncurry $ flip replicate)
       . factorise
       $ n

showStep ps =
  (text' 1 (intercalate "×" . map show $ (ps ++ [1])) # alignR)
  |||
  strutX 0.5
  |||
  factorDiagram' ps # mkBox 1

mkBox r d = d # sized (Dims r r) <> square r # lw none

-- Put it all together

recursion =
  hsep 3
  [ vsep 2
    [ abstract 3
    , abstract 5
    ]
    # centerXY
  , build 60 # scale 2 # centerXY
  ]
  # centerXY

----------------------------------------------------------------------
-- Explain the color scheme
----------------------------------------------------------------------

showDefaultColors = hcat $ zipWith showColor defaultColors [0..]
  where
    showColor c d = text' 2 (show d) <> square 1 # fc c # lw none

showPrimeColors p =
    vsep 0.3
    [ factorDiagram p # mkBox 2
    , text' 1 (show p)
    ]
  where
    ds = map (:[]) . show $ p
    dSize = 2 / fromIntegral (length ds)
    poly = polygon with { _polyType = PolyRegular (fromIntegral p) 1
                        , _polyOrient = OrientH
                        }

colorScheme =
  vsep 1
  [ showDefaultColors # centerXY
  , hsep 1
    [ showPrimeColors 17
    , showPrimeColors 43
    , showPrimeColors 59
    ] # centerX
  ]
  # centerXY

----------------------------------------------------------------------
-- The entire key
----------------------------------------------------------------------

key = hsep 5 [recursion, colorScheme # scale 2]
    # centerXY

----------------------------------------------------------------------
-- Credits
----------------------------------------------------------------------

footer =
  hsep 4
  [ text' 1 "© 2012-13 Brent A. Yorgey, CC-BY-3.0"
  , text' 1 "mathlesstraveled.com/factorization"
  , text' 1 "projects.haskell.org/diagrams"
  ]
  # centerXY

----------------------------------------------------------------------
-- Main poster
----------------------------------------------------------------------

grid = fdGrid (chunksOf 10 $ [1..120])

fdGrid' = vcat . map hcat . (map . map) (enbox 1 . factorDiagram)

enbox n d = d # centerXY # sized (Dims (0.8*n) (0.8*n)) <> square n # lw none

poster =
  vsep 3
  [ grid # scale 5 # centerXY
  , key
  , footer
  ]

----------------------------------------------------------------------
-- Main
----------------------------------------------------------------------

-- main = defaultMain colorScheme

main = defaultMain (poster # centerXY # pad 1.1)

benchDiagram = (poster # centerXY # pad 1.1)
