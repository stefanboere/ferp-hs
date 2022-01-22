{-# LANGUAGE OverloadedStrings #-}
module Components.Costing
  ( costingGroup
  , simpleCosting
  , cardCosting
  , costingStyle
  , BudgetPosting(..)
  )
where

import           Clay
import           Data.Default
import           Control.Monad.Fix              ( MonadFix )
import           Hledger
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Reflex.Dom              hiding ( Link(..)
                                                , rangeInput
                                                )

import           Components.Accordion
import           Components.Card

costingStyle :: Css
costingStyle = ".card-costing" ? do
  ".stack-row" ? do
    "grid-template-columns" -: "3fr 1fr"

  ".stack-content" ? do
    "justify-self" -: "flex-end"

data BudgetPosting = BudgetPosting
  { _bp_account :: AccountName
  , _bp_comment :: Text
  , _bp_tags :: [Tag]
  , _bp_amount :: MixedAmount
  }

instance Default BudgetPosting where
  def = BudgetPosting { _bp_account = ""
                      , _bp_comment = ""
                      , _bp_tags    = []
                      , _bp_amount  = mixed []
                      }

costingGroup
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t Text
  -> Dynamic t [BudgetPosting]
  -> m (Dynamic t MixedAmount)
costingGroup lbl ps = do
  _ <-
    stackview
      never
      (stackviewRow (dynText lbl) (dynText (showMixedAmountText <$> total)))
    $ simpleList ps
    $ \bp -> do
        stackviewRow (budgetPostingTitle bp)
                     (dynText (showMixedAmountText . _bp_amount <$> bp))
  pure total
  where total = sum . fmap _bp_amount <$> ps

budgetPostingTitle
  :: (DomBuilder t m, PostBuild t m) => Dynamic t BudgetPosting -> m ()
budgetPostingTitle bp = do
  dynText (_bp_account <$> bp)
  el "br" blank
  el "i" $ dynText (_bp_comment <$> bp)

showMixedAmountText :: MixedAmount -> Text
showMixedAmountText = Text.pack . showMixedAmountOneLine

simpleCosting
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t BudgetPosting
  -> m (Dynamic t MixedAmount)
simpleCosting bp = do
  stackviewEmpty (budgetPostingTitle bp)
                 (dynText (showMixedAmountText . _bp_amount <$> bp))
  pure (_bp_amount <$> bp)


cardCosting
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => m (Dynamic t MixedAmount)
  -> m ()
cardCosting cnt = card $ do
  cardHeader (text "Costing")
  elClass "div" "card-content card-costing" $ do
    r <- cnt
    _ <- stackviewEmpty (text "Total") (dynText (showMixedAmountText <$> r))
    pure ()

