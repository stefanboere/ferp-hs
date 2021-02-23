{-# LANGUAGE OverloadedStrings #-}
module Components.Table
  ( tableDyn
  , tableStyle
  )
where

import           Prelude                 hiding ( rem
                                                , (**)
                                                )
import           Clay                    hiding ( icon )
import           Control.Monad.Fix              ( MonadFix )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           Reflex.Dom              hiding ( display
                                                , tableDynAttr
                                                )

import           Components.Class
import           Nordtheme


tableStyle :: Css
tableStyle = do
  table ? do
    borderCollapse separate
    borderSpacing nil
    border solid (px 1) grey0'
    borderRadiusAll (px 3)
    marginTop (rem (3 / 2))
    width (pct 100)

  th ? do
    borderBottom solid (px 1) grey0'
    paddingAll (rem (3 / 4))
    textAlign center

  td ? do
    borderTop solid (px 1) nord4'
    paddingAll (rem (3 / 4))
    textAlign center

  thead ? do
    th # firstChild ? borderRadius (px 3) nil nil nil

    th # lastChild ? borderRadius (px 3) nil nil nil

  tbody ? do
    backgroundColor white

    tr # firstChild ** td ? borderTopWidth nil

    tr # lastChild ** td # firstChild ? borderRadius nil nil nil (px 3)

    tr # lastChild ** td # lastChild ? borderRadius nil nil (px 3) nil



tableDyn
  :: (Ord k, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => [(Text, k -> Dynamic t r -> m v)]
  -> Dynamic t (Map k r)
  -> m
       ( Dynamic
           t
           (Map k (Element EventResult (DomBuilderSpace m) t, [v]))
       )
tableDyn columns values = do
  tableDynAttr "" columns values rowAttrs
  where rowAttrs _ = pure (constDyn mempty)

tableDynAttr
  :: (Ord k, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text                                   -- ^ Class applied to <table> element
  -> [(Text, k -> Dynamic t r -> m v)]      -- ^ Columns of (header, row key -> row value -> child widget)
  -> Dynamic t (Map k r)                      -- ^ Map from row key to row value
  -> (k -> m (Dynamic t (Map Text Text))) -- ^ Function to compute <tr> element attributes from row key
  -> m
       ( Dynamic
           t
           ( Map
               k
               (Element EventResult (DomBuilderSpace m) t, [v])
           )
       )        -- ^ Map from row key to (El, list of widget return values)
tableDynAttr klass cols' dRows rowAttrs =
  elAttr "div" (Map.singleton "style" "zoom: 1; overflow: auto;")
    $ elAttr "table" (Map.singleton "class" klass)
    $ do
        el "thead" $ el "tr" $ mapM_ (\(h, _) -> el "th" $ text h) cols'
        el "tbody" $ listWithKey
          dRows
          (\k r -> do
            dAttrs <- rowAttrs k
            elDynAttr' "tr" dAttrs $ mapM (\x -> el "td" $ snd x k r) cols'
          )

