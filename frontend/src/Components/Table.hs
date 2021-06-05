{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Components.Table
  ( tableDyn
  , tableStyle
  , tableEl
  , datagrid
  , tableAttr
  , sortlabel
  , filterEl
  , columnHead
  , paginationInput
  , tfooter
  , rowMultiSelect
  , selectedCountInfo
  , showHideColumns
  , linkCell
  , angleDoubleRightIcon
  )
where

import           Prelude                 hiding ( rem
                                                , (**)
                                                , span
                                                )
import           Clay                    hiding ( icon )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Default
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Reflex.Dom              hiding ( display
                                                , tableDynAttr
                                                )

import           Components.Button
import           Components.Input.Basic
import           Components.Icon
import           Components.Class
import           Nordtheme


tableStyle :: Css
tableStyle = do
  selectedCountStyle
  (  ".datagrid"
    ** th
    <> ".datagrid-1"
    ** tr
    ** td
    #  firstChild
    <> ".datagrid-2"
    ** tr
    ** td
    #  nthChild "2"
    )
    ? do
        position relative
        before Clay.& do
          content (stringContent "")
          position absolute
          height (pct 100 @-@ rem (1 / 2))
          width (px 1)
          top (rem (1 / 4))
          right nil
          backgroundColor nord4'

  (".datagrid-1" <> ".datagrid-2") ? do
    tr ** (td <> th) # firstChild ? width (rem (3 / 2))

  ".datagrid-2" ? do
    tr ** (td <> th) # nthChild "2" ? width (rem (3 / 2))

  ".datagrid" ? do
    tbody ** tr # lastChild ** td ? borderBottomWidth 1
    th # before ? backgroundColor (lighten 0.5 grey0')
    th # lastChild # before ? display none

  table ? do
    borderCollapse separate
    borderSpacing nil
    border solid (px 1) grey0'
    borderRadiusAll (px 3)
    marginTop (rem (3 / 2))
    width (pct 100)
    overflow scroll

  ".pagination" ? do
    display flex
    marginLeft auto
    input ? width (rem 2)
    span ? do
      marginLeft (rem 1)
      marginRight (rem (1 / 2))

    Clay.button # disabled ? do
      backgroundColor inherit
      "fill" -: showColor (lighten 0.5 grey0')

  ".show-hide-columns" ? do
    marginTop (rem 1)
    marginBottom (rem 1)


  ".sortlabel" ? do
    display flex
    cursor pointer
    hover Clay.& textDecoration underline

    ".hidden" ? do
      important $ display none

  th ? do
    Clay.button ? height (rem 1)
    span ? flexGrow 1
    borderBottom solid (px 1) grey0'
    paddingAll (rem (1 / 2))
    lineHeight (rem 1)
    textAlign start
    backgroundColor nord4'

  td ? do
    ".right" Clay.& textAlign end
    ".center" Clay.& textAlign center
    paddingAll (rem (1 / 2))
    textAlign start
    ".input" ? do
      marginTop (rem (-1 / 4))
      marginBottom (rem (-1 / 2))

    input # ("type" @= "checkbox") ? marginTop (rem (-1 / 8))

    input ? do
      borderBottomColor nord4'
      height (rem 1)
      width (rem 1)
    Clay.button ? do
      important
        $ margin (rem (-1 / 4)) (rem (1 / 4)) (rem (-1 / 4)) (rem (1 / 4))
      paddingAll (rem (1 / 4))

  (thead <> tbody <> tfoot) ** tr ? do
    "display" -: "table"
    width (pct 100)
    "table-layout" -: "fixed"

  thead ? do
    th # firstChild ? borderRadius (px 3) nil nil nil

    th # lastChild ? borderRadius (px 3) nil nil nil

  tbody ? do
    display block
    overflow auto
    backgroundColor white
    tr # lastChild ** td ? borderBottomWidth nil

    tr # hover ? backgroundColor nord6'

    tr # ".active" ? backgroundColor nord4'


  (tbody <> thead) ** td ? borderBottom solid (px 1) nord4'

  (tbody <> tfoot) ? do

    tr # lastChild ** td # firstChild ? borderRadius nil nil nil (px 3)

    tr # lastChild ** td # lastChild ? borderRadius nil nil (px 3) nil

  tfoot ? do
    td ? borderTop solid (px 1) nord4'

    tr # lastChild ** td ? borderTopColor grey0'



tableDyn
  :: (Ord k, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => [(m (), k -> Dynamic t r -> m v)]
  -> Dynamic t (Map k r)
  -> m
       ( Dynamic
           t
           (Map k (Element EventResult (DomBuilderSpace m) t, [v]))
       )
tableDyn columns values = tableDynAttr "" columns values rowAttrs
  where rowAttrs _ = pure (constDyn mempty)

tableDynAttr
  :: (Ord k, DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Text                                   -- ^ Class applied to <table> element
  -> [(m (), k -> Dynamic t r -> m v)]      -- ^ Columns of (header, row key -> row value -> child widget)
  -> Dynamic t (Map k r)                    -- ^ Map from row key to row value
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
  tableAttr (Map.singleton "class" klass) $ do
    el "thead" $ el "tr" $ mapM_ (\(h, _) -> el "th" h) cols'
    el "tbody" $ listWithKey
      dRows
      (\k r -> do
        dAttrs <- rowAttrs k
        elDynAttr' "tr" dAttrs $ mapM (\x -> el "td" $ snd x k r) cols'
      )

tableAttr :: DomBuilder t m => Map Text Text -> m a -> m a
tableAttr attrs =
  elAttr "div" (Map.singleton "style" "zoom: 1; overflow: auto;")
    . elAttr "table" attrs

tableEl :: DomBuilder t m => m a -> m a
tableEl = tableAttr Map.empty

datagrid :: DomBuilder t m => Int -> m a -> m a
datagrid i' = tableAttr ("class" =: ("datagrid datagrid-" <> pack (show i')))

linkCell :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m a
linkCell dynHref =
  elClass "td" "center" . elDynAttr "a" (Map.singleton "href" <$> dynHref)

angleDoubleRightIcon :: (DomBuilder t m, PostBuild t m) => m ()
angleDoubleRightIcon =
  icon def { _iconConfig_direction = constDyn DirRight } angleDoubleIcon

data SortOrder = Descending | Ascending deriving (Eq, Show)

sortlabel
  :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m)
  => Text
  -> m (Dynamic t (Maybe SortOrder))
sortlabel lbl = do
  rec (e, _) <- elClass' "span" "sortlabel" $ do
        el "span" $ text lbl
        icon
          IconConfig { _iconConfig_direction = mkDir <$> dynSortOrd
                     , _iconConfig_status    = constDyn (Just Info)
                     , _iconConfig_size      = 1
                     , _iconConfig_class     = mkCls <$> dynSortOrd
                     }
          arrowIcon

      let clickEv = domEvent Click e
      dynSortOrd <- fmap readSort <$> count clickEv

  pure dynSortOrd

 where
  mkDir (Just Descending) = DirDown
  mkDir _                 = DirUp

  mkCls Nothing = Just "hidden"
  mkCls _       = Nothing

  readSort i' = case i' `mod` (3 :: Int) of
    1 -> Just Ascending
    2 -> Just Descending
    _ -> Nothing

filterEl
  :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m)
  => TooltipPosition
  -> Dynamic t Bool
  -> m a
  -> m a
filterEl pos isSetDyn cnt = do
  let dynIco =
        icon def { _iconConfig_status = mkStatus <$> isSetDyn }
             (dyn (mkIco <$> isSetDyn))
          >> pure ()
  (x, _) <- signpost' dynIco pos cnt
  pure x
 where
  mkIco True  = filterGridCircleIcon
  mkIco False = filterGridIcon

  mkStatus True  = Just Info
  mkStatus False = Nothing

showHideColumns
  :: ( MonadIO m
     , MonadFix m
     , MonadHold t m
     , DomBuilder t m
     , PostBuild t m
     , Ord k
     )
  => Map k Text
  -> m (Dynamic t (Set k))
showHideColumns columns = do
  (x, _) <- signpost' ico TopRight $ do
    el "h3" $ text "Show columns"
    let allCols = Map.keysSet columns
    rec dynSet <- elClass "div" "show-hide-columns" $ checkboxesInputMap
          columns
          (inputConfig allCols) { _inputConfig_setValue = allCols <$ selectAllEv
                                }
        selectAllEv <- btn def { _buttonConfig_priority = ButtonTertiary }
          $ text "Select all"
    pure dynSet

  pure (_inputEl_value x)
  where ico = icon def viewColumnsIcon

columnHead :: (DomBuilder t m) => m a -> m a
columnHead = el "th" . elClass "div" "flex-row"

data Page = Page
  { _page_num :: Int
  , _page_size :: Int
  }
  deriving (Eq, Show)

data PageSize = Page10 | Page20 | Page50 | Page100 deriving (Eq, Show, Enum, Bounded, Ord)

instance Default PageSize where
  def = Page10

instance HasLabel PageSize where
  toLabel = pack . show . pageSize

pageSize :: PageSize -> Int
pageSize Page10  = 10
pageSize Page20  = 20
pageSize Page50  = 50
pageSize Page100 = 100

tfooter :: DomBuilder t m => m a -> m a
tfooter =
  el "tr" . elAttr "td" ("colspan" =: "1000") . elClass "div" "flex-row"

rowMultiSelect
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => Bool
  -> Event t Bool
  -> m a
  -> m (Dynamic t Bool, a)
rowMultiSelect fullRowSelect setSelectEv cnt = do
  rec (e, (r, x)) <- elDynClass' "tr" (selectedCls <$> dynSel) $ do
        (r', _) <- elClass' "td" "row-select"
          $ checkboxInputSimple False (updated dynSel) mempty
        x' <- cnt
        pure (r', x')
      let rowClickEv = domEvent Click $ if fullRowSelect then e else r
      dynSel <- foldDyn ($) False
        $ leftmost [const <$> setSelectEv, Prelude.not <$ rowClickEv]

  pure (dynSel, x)
 where
  selectedCls True  = "active"
  selectedCls False = ""

selectedCountStyle :: Css
selectedCountStyle = do
  ".hidden" ? display none

  ".selected-count" ? do
    marginLeft (rem (1 / 4))
    cursor cursorDefault
    before Clay.& do
      backgroundColor grey0'
      Clay.display inlineBlock
      marginRight (rem (1 / 2))
      marginBottom (rem (-1 / 4))
      position relative

selectedCountInfo :: (PostBuild t m, DomBuilder t m) => Dynamic t Int -> m ()
selectedCountInfo dynCount = elDynClass "div" (mkCls <$> dynCount)
  $ dynText (pack . show <$> dynCount)
 where
  mkCls x | x > 0     = "selected-count"
          | otherwise = "selected-count hidden"

paginationInput
  :: (MonadHold t m, MonadFix m, PostBuild t m, DomBuilder t m)
  => Dynamic t (Maybe Int)
  -> m (Dynamic t Page)
paginationInput totalResults = elClass "div" "pagination" $ do
  el "span" $ text "Results per page"
  dynLim <- _inputEl_value <$> selectInput (inputConfig (Just Page10))
  let pageSizeDyn = pageSize' <$> dynLim
  let maxPageDyn  = maxPage <$> totalResults <*> pageSizeDyn
  rec
    el "span" $ dynText
      (resultSummary <$> pageSizeDyn <*> pageNumWithDef <*> totalResults)
    let btnPrevStateDyn = btnPrevState <$> pageNumWithDef
    prevAllEv <- btn
      def { _buttonConfig_priority = ButtonTertiary
          , _buttonConfig_state    = btnPrevStateDyn
          }
      (icon def { _iconConfig_direction = constDyn DirDown } stepForwardIcon)
    prevEv <- btn
      def { _buttonConfig_priority = ButtonTertiary
          , _buttonConfig_state    = btnPrevStateDyn
          }
      (icon def { _iconConfig_direction = constDyn DirLeft } angleIcon)
    pageNum <- integralInput (inputConfig'
                               (NumberRange
                                 { _numberRange_minValue  = constDyn (Just 1)
                                 , _numberRange_maxValue  = maxPageDyn
                                 , _numberRange_precision = Just 0
                                 }
                               )
                               (1 :: Int)
                             )
      { _inputConfig_setValue = leftmost
        [ 1 <$ prevAllEv
        , decrement <$> tagPromptlyDyn pageNumWithDef prevEv
        , (+ 1) <$> tagPromptlyDyn pageNumWithDef nextEv
        , fmapMaybe Prelude.id $ tagPromptlyDyn maxPageDyn nextAllEv
        ]
      }
    let pageNumWithDef = fromMaybe 1 <$> _inputEl_value pageNum
    dynText $ maybe "" (("/ " <>) . pack . show) <$> maxPageDyn
    let btnNextStateDyn = btnNextState <$> pageNumWithDef <*> maxPageDyn
    nextEv <- btn
      def { _buttonConfig_priority = ButtonTertiary
          , _buttonConfig_state    = btnNextStateDyn
          }
      (icon def { _iconConfig_direction = constDyn DirRight } angleIcon)
    let btnNextAllStateDyn = btnNextAllState <$> pageNumWithDef <*> maxPageDyn
    nextAllEv <- btn
      def { _buttonConfig_priority = ButtonTertiary
          , _buttonConfig_state    = btnNextAllStateDyn
          }
      (icon def { _iconConfig_direction = constDyn DirUp } stepForwardIcon)
  pure $ Page <$> pageNumWithDef <*> pageSizeDyn

 where
  btnPrevState x | x <= 1    = ActionDisabled
                 | otherwise = ActionAvailable

  btnNextState x (Just m) | x >= m    = ActionDisabled
                          | otherwise = ActionAvailable
  btnNextState _ _ = ActionAvailable

  btnNextAllState x (Just m) | x >= m    = ActionDisabled
                             | otherwise = ActionAvailable
  btnNextAllState _ _ = ActionDisabled

  decrement x = x - 1

  pageSize' (Just z) = pageSize z
  pageSize' Nothing  = def

  maxPage (Just total) pageSz =
    Just $ ceiling $ (fromIntegral total :: Double) / fromIntegral pageSz
  maxPage Nothing _ = Nothing

  pageRange pageSz pageNum = (pageSz * (pageNum - 1) + 1, pageSz * pageNum)

  resultSummary pageSz pageNum (Just total) =
    let (r0, r1) = pageRange pageSz pageNum
    in  pack (show (Prelude.min r0 total))
          <> "-"
          <> pack (show (Prelude.min r1 total))
          <> " of "
          <> pack (show total)
          <> " results"
  resultSummary pageSz pageNum Nothing =
    let (r0, r1) = pageRange pageSz pageNum
    in  pack (show r0) <> "-" <> pack (show r1)

