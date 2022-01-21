{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Components.Input.Numeric
  ( Overridable(..)
  , overridableNumberInput
  , rangeAndNumberInput
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Data.Default
import           Data.Monoid                    ( First(..) )
import           Data.Maybe                     ( isJust )
import           Reflex
import           Reflex.Dom              hiding ( rangeInput )

import           Components.Input.Basic

data Overridable a = Overridable
  { ovr_calculation :: a
  , ovr_value       :: Maybe a
  }
  deriving (Eq, Show)

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
     )
  => Event t a
  -> InputConfig t m (Overridable a)
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


-- | Range input with a number input for specific value input
rangeAndNumberInput
  :: ( MonadHold t m
     , PostBuild t m
     , DomBuilder t m
     , MonadFix m
     , Read a
     , RealFloat a
     )
  => NumberInputConfig t m a
  -> m (DomInputEl t m (Maybe a))
rangeAndNumberInput = boundNumericInput $ \cfg ->
  inputGroup (inputConfig (_inputConfig_id cfg) ())
      { _inputConfig_status = _inputConfig_status cfg
      }
    $ do
        rec
          rel <- mkElem cfg False "_range" (exceptSelf nel nel_ev)
          nel <- mkElem cfg True "_number" (exceptSelf rel rel_ev)
-- This is an ugly way of making sure the prerendering doesn't loop infinitely
-- We cannot pass the events directly to the other mkElem, because then the prerender stops working.
-- Instead we manually setup the events
          pb  <- headE $ leftmost
            [updated $ _inputEl_hasFocus rel, updated $ _inputEl_hasFocus nel]
          rel_ev <- switchHold
            never
            (fmapMaybe id (updated (_inputEl_value rel)) <$ pb)
          nel_ev <- switchHold
            never
            (fmapMaybe id (updated (_inputEl_value nel)) <$ pb)
        pure $ fmap getFirst $ fmap First nel <> fmap First rel

 where
  exceptSelf other self = gate (current $ _inputEl_hasFocus other) self
  mkElem cfg isReg idSuff setEv = numericInputInternal
    isReg
    cfg { _inputConfig_status   = def
        , _inputConfig_id       = _inputConfig_id cfg <> idSuff
        , _inputConfig_setValue = leftmost [_inputConfig_setValue cfg, setEv]
        }
