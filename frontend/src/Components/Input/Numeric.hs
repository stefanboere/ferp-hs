{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Components.Input.Numeric
  ( Overridable(..)
  , overridableNumberInput
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Default
import           Data.Maybe                     ( isJust )
import           Reflex
import           Reflex.Dom

import           Components.Input.Basic

data Overridable a = Overridable
  { ovr_calculation :: a
  , ovr_value :: Maybe a
  } deriving (Eq, Show)

overridableValue :: Overridable a -> a
overridableValue (Overridable _ (Just x)) = x
overridableValue (Overridable x _       ) = x

isOverridden :: Overridable a -> Bool
isOverridden (Overridable _ x) = isJust x

instance Functor Overridable where
  fmap f (Overridable a b) = Overridable (f a) (fmap f b)

instance Default a => Default (Overridable a) where
  def = Overridable def def


overridableNumberInput
  :: ( MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , Read a
     , Show a
     , MonadIO m
     )
  => InputConfig t (Overridable a)
  -> m (Dynamic t (Maybe (Overridable a)))
overridableNumberInput cfg = labeled cfg $ \idStr _ -> el "div" $ do
  rec
    dynMVal <- numberInput'
      idStr
      (fmap overridableValue cfg)
        { _inputConfig_status   = numStatus
                                  <$> _inputConfig_status cfg
                                  <*> dynOverridden
        , _inputConfig_setValue = leftmost
          [ overridableValue <$> _inputConfig_setValue cfg
          , attachPromptlyDynWith (\x _ -> ovr_calculation x) calc
            $ ffilter not (updated dynOverridden)
          ]
        }
    dynOverridden <- toggleInput (fmap isOverridden cfg)
      { _inputConfig_label  = constDyn "Override"
      , _inputConfig_status = overriddenStatus <$> _inputConfig_status cfg
      }
    calc <- holdDyn (_inputConfig_initialValue cfg) (_inputConfig_setValue cfg)

  pure $ overridable <$> fmap ovr_calculation calc <*> dynOverridden <*> dynMVal
 where
  overridable :: a -> Bool -> Maybe a -> Maybe (Overridable a)
  overridable _ True  Nothing    = Nothing
  overridable d True  x@(Just _) = Just $ Overridable d x
  overridable d False _          = Just $ Overridable d Nothing

  numStatus :: InputStatus -> Bool -> InputStatus
  numStatus x True  = x
  numStatus _ False = InputDisabled

  overriddenStatus :: InputStatus -> InputStatus
  overriddenStatus InputDisabled = InputDisabled
  overriddenStatus _             = InputNeutral Nothing









