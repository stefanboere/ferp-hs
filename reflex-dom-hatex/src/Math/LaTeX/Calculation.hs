{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Math.LaTeX.Calculation
Description: EDSL for expressing calculations and obtaining intermediate results
Stability: experimental

TODO Extract this to separate package. Included here for rapid development

-}
module Math.LaTeX.Calculation
  ( example
  )
where


import           CAS.Dumb
import           Data.Ratio
import           Math.LaTeX.Prelude
import qualified Text.LaTeX                    as TeX
import qualified Text.LaTeX.Packages.AMSMath   as TeX
import qualified Text.LaTeX.Packages.AMSSymb   as TeX

type Input
  = CAS' Int (Infix TeX.LaTeX) (Encapsulation TeX.LaTeX) (Symbol TeX.LaTeX)

example :: Monad m => Float -> TeX.LaTeXT m ()
example x = do
  TeX.documentclass [] TeX.article
  TeX.title "Symbolic HaTeX"
  TeX.usepackage [] TeX.amsmath
  TeX.usepackage [] TeX.amssymb
  TeX.document $ do
    TeX.maketitle
    TeX.section "Section 1"
    "The following is an experiment in writing worked calculations with TeX-my-math."
    _ <- dcalculation
      ( (𝑎 ⩵ someProperty symbs &~~: declaredInputs &~: ㄚ / 1 :=: ㄚ)
      ⩵ numberAsCas (someProperty inputs)
      )
      "."
    pure ()

 where
  inputs :: Torus Float
  inputs = Torus { _mayorRadius = 0.1 + x, _minorRadius = 0.4 }

  declaredInputs :: [Input]
  declaredInputs = declareInputs inputs


data Torus a = Torus
  { _mayorRadius :: a
  , _minorRadius :: a
  }


declareInputs :: RealFrac a => Torus a -> [Input]
declareInputs vals =
  [ _mayorRadius symbs ⩵ numberAsCas (_mayorRadius vals)
  , _minorRadius symbs ⩵ numberAsCas (_minorRadius vals)
  ]

numberAsCas :: (RealFrac a, Fractional b) => a -> b
numberAsCas x = fromRational (approxRational x 0.001)

symbs :: Torus (Expression' γ s² s¹ ζ)
symbs = Torus { _mayorRadius = 𝑅, _minorRadius = 𝑟 }

someProperty :: (Fractional a) => Torus a -> a
someProperty Torus {..} = _mayorRadius / _minorRadius

