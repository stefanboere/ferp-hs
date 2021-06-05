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
     , RealFloat a
     , MonadIO m
     )
  => Event t a
  -> InputConfig t (Overridable a)
  -> m (DomInputEl t m (Maybe (Overridable a)))
overridableNumberInput setCalc cfg = elClass "div" "flex-row" $ do
  rec
    dynMVal <- numberInput (fmap overridableValue cfg)
      { _inputConfig_status   = numStatus
                                <$> _inputConfig_status cfg
                                <*> dynOverridden
      , _inputConfig_setValue = leftmost
                                  [ overridableValue
                                    <$> _inputConfig_setValue cfg
                                  , attachPromptlyDynWith const calc
                                    $ ffilter not (updated dynOverridden)
                                  , gate (not <$> current dynOverridden) setCalc
                                  ]
      , _inputConfig_extra    = def { _numberRange_precision = Just 3 }
      }
    dynOverriddenEl <- toggleInput
      "Override"
      (fmap isOverridden cfg)
        { _inputConfig_status = overriddenStatus <$> _inputConfig_status cfg
        }
    let dynOverridden = _inputEl_value dynOverriddenEl
    calc <- holdDyn
      (ovr_calculation $ _inputConfig_initialValue cfg)
      (leftmost [ovr_calculation <$> _inputConfig_setValue cfg, setCalc])

  pure $ overridable <$> hiddenInput calc <*> dynOverriddenEl <*> dynMVal
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









