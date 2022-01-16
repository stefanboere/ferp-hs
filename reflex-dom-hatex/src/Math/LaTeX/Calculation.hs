{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module: Math.LaTeX.Calculation
Description: EDSL for expressing calculations and obtaining intermediate results
Stability: experimental

TODO Extract this to separate package. Included here for rapid development

-}
module Math.LaTeX.Calculation
  ( asPattern
  , formula
  , Variables
  , Variable
  , module Math.LaTeX.Prelude
  )
where

import           CAS.Dumb
import           CAS.Dumb.Tree
import           Data.Void                      ( Void )
import           GHC.Generics
import           Math.LaTeX.Prelude
import           Numeric                        ( showGFloat )
import qualified Text.LaTeX                    as TeX

type Variable γ
  = CAS' γ (Infix TeX.LaTeX) (Encapsulation TeX.LaTeX) (Symbol TeX.LaTeX)

asPattern :: Variable Void -> Variable Int
asPattern (Symbol x      ) = Symbol x
asPattern (Function f x  ) = Function f (asPattern x)
asPattern (Operator f x y) = Operator f (asPattern x) (asPattern y)
asPattern (OperatorChain x xs) =
  OperatorChain (asPattern x) (fmap (fmap asPattern) xs)
asPattern (Gap _) = Gap 0 -- Cannot happen, but here to please the compiler

formula
  :: (Monad m, Variables f, RealFloat b)
  => (forall γ . f (Variable γ))
  -> (forall γ s¹ s² ζ . Expression' γ s² s¹ ζ)
  -> (forall a . Floating a => f a -> a)
  -> f b
  -> TeX.LaTeXT m (Variable Void)
formula s symb calc inp = dcalculation
  ((symb ⩵ calc s &~~: ds &~: ㄚ / 1 :=: ㄚ) ⩵ numberAsCas (calc inp))
  "."
  where ds = variables s inp

class Variables r where
  variables :: RealFloat a => r (Variable Int)  -> r a -> [Variable Int]
  default variables :: (Generic1 r, GVariables (Rep1 r), RealFloat a) => r (Variable Int) -> r a -> [Variable Int]
  variables x y = gVariables (from1 x) (from1 y)

class GVariables (r :: * -> *) where
  gVariables :: RealFloat a => r (Variable Int) -> r a -> [Variable Int]

instance GVariables f
  => GVariables (M1 a k f) where
  gVariables x y = gVariables (unM1 x) (unM1 y)

instance (GVariables f, GVariables g)
  => GVariables (f :*: g) where
  gVariables (x0 :*: x1) (y0 :*: y1) = gVariables x0 y0 ++ gVariables x1 y1

instance (GVariables f, GVariables g) => GVariables (f :+: g) where
  gVariables (L1 x) (L1 y) = gVariables x y
  gVariables (R1 x) (R1 y) = gVariables x y
  gVariables _      _      = []

instance GVariables Par1 where
  gVariables s v = [unPar1 s ⩵ numberAsCas (unPar1 v)]

numberAsCas :: (RealFloat a) => a -> Variable γ
numberAsCas x =
  Symbol (StringSymbol $ TeX.fromString $ showGFloat (Just 3) x "")
  --fromRational (approxRational x 0.001)
