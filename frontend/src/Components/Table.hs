{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
module Components.Table
  ( tableDyn
  , tableStyle
  , tableEl
  , datagrid
  , datagridDyn
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
  , withFilterCondition
  , headMultiSelect
  , Column(..)
  , MapSubset(..)
  , DatagridConfig(..)
  , DatagridResult(..)
  , Page(..)
  , pageSize
  , PageSize
  , ViewWindow(..)
  , SortOrder(..)
  , addDeletes
  , FilterCondition(..)
  ) where

import           Clay                    hiding ( icon )
import           Control.Lens                   ( (^.) )
import           Control.Monad                  ( join )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Default
import           Data.Fixed
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Language.Javascript.JSaddle    ( js1
                                                , jsg
                                                , jss
                                                , liftJSM
                                                )
import           Prelude                 hiding ( (**)
                                                , rem
                                                , span
                                                )
import           Reflex.Dom              hiding ( display
                                                , tableDynAttr
                                                )
import           Servant.API                    ( toUrlPiece )
import qualified Servant.Links                 as L
                                                ( Link
                                                , linkURI
                                                )
import           URI.ByteString                 ( URI )

import           Components.Button
import           Components.Class
import           Components.Icon
import           Components.Input.Basic
import           Components.Navigation
import           Nordtheme


tableStyle :: Css
tableStyle = do
  ".flex-center" ? do
    display flex
    alignItems center

  ".datagrid-wrapper" ? do
    "zoom" -: "1"
    overflow auto
    display flex
    height (pct 100)

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
    marginTop (rem (1 / 2))
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

  (".dropdown-menu" <> ".combobox-menu") ? do
    backgroundColor white

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
    ".link" Clay.& do
      paddingAll nil
      important $ width (rem (5 / 2))
      a ? do
        width (rem (3 / 2))
        display inlineBlock
        paddingAll (rem (1 / 2))

    paddingAll (rem (1 / 2))
    textAlign start
    ".input" ? do
      marginTop (rem (-1 / 4))
      marginBottom (rem (-1 / 2))

    input # ("type" @= "checkbox") ? marginTop (rem (-1 / 8))

    (Clay.select <> ".select") ? do
      borderBottomColor nord4'

    input ? do
      borderBottomColor nord4'
      height (rem 1)
      width (rem 1)

    ".dropdown-menu" ? Clay.button ? do
      important $ marginAll nil
      padding nil (rem 1) nil (rem 1)

    ".dropdown-menu" ? do
      top (rem (3 / 2))

    ".dropdown-select" ? do
      paddingRight nil
      important $ borderWidth nil
      hover Clay.& do
        important $ fontColor nord3'
        important $ "fill" -: showColor nord3'

  thead ** td ** Clay.button ? do
    important $ margin (rem (-1 / 4)) (rem (1 / 4)) (rem (-1 / 4)) (rem (1 / 4))
    paddingAll (rem (1 / 4))

  tfoot ** ".signpost" ? do
    marginTop (rem (-1 / 4))
    marginBottom (rem (-1 / 4))

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

    tr ? do
      overflow hidden
      whiteSpace nowrap
      height (px 36)
      (td <> th) ? do
        overflow hidden
        textOverflow overflowEllipsis

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

datagrid
  :: (DomBuilder t m, Prerender js t m)
  => Int
  -> (Dynamic t (Maybe Double) -> m a)
  -> m a
datagrid i' cnt = elClass "div" "datagrid-wrapper" $ do
      -- resizeDetector is meant to contain the content you want to know the size of
      -- However, we'd need to put the entire datagrid in the prerender.
      -- To avoid this, since we only need the height anyway, we put it on the left
  dynHeight <- prerender (pure (constDyn Nothing)) $ do
    (resizeEv, _) <- resizeDetectorWithStyle "margin-right:-1px;"
      $ elAttr "div" (Map.singleton "style" "width:1px") blank
    holdDyn Nothing (snd <$> resizeEv)

  elClass "table" ("datagrid datagrid-" <> pack (show i'))
    $ cnt (join dynHeight)

linkCell :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m a
linkCell dynHref = elClass "td" "center link" . elDynAttr
  "a"
  ((Map.singleton "tabindex" "-1" <>) . Map.singleton "href" <$> dynHref)

safelinkCell
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t L.Link
  -> m ()
  -> m (Event t URI)
safelinkCell lnk cnt = do
  clickEv <- elClass "td" "center link" $ ahrefPreventDefault
    (("/" <>) . toUrlPiece <$> lnk)
    (constDyn False)
    (Map.singleton "tabindex" "-1")
    cnt
  pure $ tagPromptlyDyn (coerceUri . L.linkURI <$> lnk) clickEv

angleDoubleRightIcon :: (DomBuilder t m, PostBuild t m) => m ()
angleDoubleRightIcon =
  icon def { _iconConfig_direction = constDyn DirRight } angleDoubleIcon

data SortOrder = Descending | Ascending deriving (Eq, Show)

sortlabel
  :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -> Maybe SortOrder
  -> Event t (Maybe SortOrder)
  -> m (Dynamic t (Maybe SortOrder))
sortlabel dynLbl initVal updateEv = do
  rec (e, _) <- elClass' "span" "sortlabel" $ do
        el "span" $ dynText dynLbl
        icon
          IconConfig { _iconConfig_direction = mkDir <$> dynSortOrd
                     , _iconConfig_status    = constDyn (Just Info)
                     , _iconConfig_size      = 1
                     , _iconConfig_class     = mkCls <$> dynSortOrd
                     }
          arrowIcon

      let clickEv = domEvent Click e
      dynSortOrd <- foldDyn ($) initVal
        $ leftmost [const <$> updateEv, cycle' <$ clickEv]

  pure dynSortOrd

 where
  mkDir (Just Descending) = DirDown
  mkDir _                 = DirUp

  mkCls Nothing = Just "hidden"
  mkCls _       = Nothing

  cycle' Nothing           = Just Ascending
  cycle' (Just Ascending ) = Just Descending
  cycle' (Just Descending) = Nothing

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
          (fmap text columns)
          (inputConfig allCols) { _inputConfig_setValue = allCols <$ selectAllEv
                                }
        selectAllEv <- btn def { _buttonConfig_priority = ButtonTertiary }
          $ text "Select all"
    pure dynSet

  pure (_inputEl_value x)
  where ico = icon def viewColumnsIcon

columnHead :: (DomBuilder t m) => m a -> m a
columnHead = el "th" . elClass "div" "flex-row"

columnHead' :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m a -> m a
columnHead' visibl =
  elDynClass "th" (visibleDynClass <$> visibl) . elClass "div" "flex-row"

visibleDynClass :: Bool -> Text
visibleDynClass True  = ""
visibleDynClass False = "hidden"

data Page = Page
  { _page_num  :: Integer
  , _page_size :: Integer
  }
  deriving (Eq, Show)

instance Default Page where
  def = Page 1 (pageSize def)

data PageSize = Page10 | Page20 | Page50 | Page100 deriving (Eq, Show, Enum, Bounded, Ord)

instance Default PageSize where
  def = Page10

printPageSize :: DomBuilder t m => PageSize -> m ()
printPageSize = text . pack . show . pageSize

pageSize :: PageSize -> Integer
pageSize Page10  = 10
pageSize Page20  = 20
pageSize Page50  = 50
pageSize Page100 = 100

tfooter :: DomBuilder t m => m a -> m a
tfooter =
  el "tr" . elAttr "td" ("colspan" =: "1000") . elClass "div" "flex-row"

rowMultiSelect
  :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => Map Text Text
  -> Bool
  -> Event t Bool
  -> m a
  -> m (Dynamic t Bool, a)
rowMultiSelect attrs fullRowSelect setSelectEv cnt = do
  rec (e, (r, x)) <- elDynAttr' "tr" (selectedCls <$> dynSel) $ do
        (r', _) <- elClass' "td" "row-select"
          $ checkboxInputSimple
              False
              (updated dynSel)
              (Map.singleton "tabindex" "-1")
        x' <- cnt
        pure (r', x')
      let rowClickEv = domEvent Click $ if fullRowSelect then e else r
      dynSel <- foldDyn ($) False
        $ leftmost [const <$> setSelectEv, Prelude.not <$ rowClickEv]

  pure (dynSel, x)
 where
  selectedCls True  = attrs <> Map.singleton "class" "active"
  selectedCls False = attrs

selectedCountStyle :: Css
selectedCountStyle = do
  ".hidden" ? do
    important $ display none

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
  => Dynamic t (Maybe Integer)
  -> Page
  -> Event t Page
  -> m (Dynamic t Page)
paginationInput totalResults initPage updatePage =
  elClass "div" "pagination" $ do
    el "span" $ text "Results per page"
    dynLim <- _inputEl_value
      <$> selectInput (inputConfig' (OpElem printPageSize) (Just Page10))
    pageSizeDyn <- holdDyn (_page_size initPage)
      $ leftmost [updated (pageSize' <$> dynLim), _page_size <$> updatePage]
    let maxPageDyn = maxPage <$> totalResults <*> pageSizeDyn
    rec
      el "span" $ dynText
        (resultSummary' <$> pageSizeDyn <*> pageNumWithDef <*> totalResults)
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
                                 (_page_num initPage)
                               )
        { _inputConfig_setValue = leftmost
          [ 1 <$ prevAllEv
          , decrement <$> tag (current pageNumWithDef) prevEv
          , (+ 1) <$> tag (current pageNumWithDef) nextEv
          , fmapMaybe Prelude.id $ tag (current maxPageDyn) nextAllEv
          , _page_num <$> updatePage
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
      let btnNextAllStateDyn =
            btnNextAllState <$> pageNumWithDef <*> maxPageDyn
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

  resultSummary' pageSz pageNum = resultSummary (pageRange pageSz pageNum)

resultSummary :: (Integer, Integer) -> Maybe Integer -> Text
resultSummary (r0, r1) (Just total) =
  pack (show (Prelude.min r0 total))
    <> "-"
    <> pack (show (Prelude.min r1 total))
    <> " of "
    <> pack (show total)
    <> " results"
resultSummary (r0, r1) Nothing = pack (show r0) <> "-" <> pack (show r1)


data FilterCondition
  = Equal
  | DoesNotEqual
  | GreaterThan
  | LessThan
  | GreaterThanOrEqual
  | LessThanOrEqual
  | StartsWith
  | EndsWith
  | Contains
  | DoesNotStartWith
  | DoesNotEndWith
  | DoesNotContain
  deriving (Eq, Enum, Show, Bounded, Ord)

instance Default FilterCondition where
  def = Contains

filterConditionText :: FilterCondition -> Text
filterConditionText x = case x of
  Equal              -> "Equals"
  DoesNotEqual       -> "Does not equal"
  GreaterThan        -> "Greater than"
  LessThan           -> "Less than"
  GreaterThanOrEqual -> "Greater than or equal"
  LessThanOrEqual    -> "Less than or equal"
  StartsWith         -> "Starts with"
  EndsWith           -> "Ends with"
  Contains           -> "Contains"
  DoesNotStartWith   -> "Does not start with"
  DoesNotEndWith     -> "Does not end with"
  DoesNotContain     -> "Does not contain"

filterConditionIcon
  :: (DomBuilder t m, PostBuild t m) => FilterCondition -> m ()
filterConditionIcon x = case x of
  Equal        -> icon sml equalsIcon
  DoesNotEqual -> icon sml doesNotEqualIcon
  GreaterThan ->
    icon sml { _iconConfig_direction = constDyn DirRight } angleIcon
  LessThan -> icon sml { _iconConfig_direction = constDyn DirLeft } angleIcon
  GreaterThanOrEqual -> icon sml greaterThanOrEqIcon
  LessThanOrEqual    -> icon sml lessThanOrEqIcon
  StartsWith         -> icon lg startsWithIcon
  EndsWith           -> icon lg endsWithIcon
  Contains           -> icon lg containsIcon
  DoesNotStartWith   -> icon lg doesNotStartWithIcon
  DoesNotEndWith     -> icon lg doesNotEndWithIcon
  DoesNotContain     -> icon lg doesNotContainIcon
   where
 where
  sml = def { _iconConfig_size = 1.25 }
  lg  = def { _iconConfig_size = 1.5 }

printFilterCond :: (PostBuild t m, DomBuilder t m) => FilterCondition -> m ()
printFilterCond x =
  filterConditionIcon x >> el "span" (text (filterConditionText x))

withFilterCondition
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => [FilterCondition]
  -> FilterCondition
  -> (FilterCondition -> Event t FilterCondition -> m a)
  -> m a
withFilterCondition avail initCond editor = inputGroup def $ do
  rec selectEv <-
        btnDropdown
          def { _buttonConfig_class    = "dropdown-select select"
              , _buttonConfig_priority = ButtonTertiary
              }
          (dyn_ (filterConditionIcon <$> dynVal))
        $   leftmost
        <$> mapM
              (\c -> do
                ev <- btn def (elClass "div" "flex-center" (printFilterCond c))
                pure (c <$ ev)
              )
              avail

      dynVal <- holdDyn initCond selectEv

  editor initCond selectEv

headMultiSelect :: (DomBuilder t m) => m a -> m (Event t Bool, a)
headMultiSelect theadTr = el "tr" $ do
  selectAllDyn <- el "th"
    $ checkboxInputSimple False never (Map.singleton "tabindex" "-1")
  x <- theadTr
  pure (updated selectAllDyn, x)

data MapSubset k a = MapSubset
  { _ms_data       :: Map k a
  , _ms_totalCount :: Maybe Integer
  }

instance Default (MapSubset k a) where
  def = MapSubset Map.empty Nothing

instance Functor (MapSubset k) where
  fmap f x = x { _ms_data = fmap f (_ms_data x) }

data DatagridResult t a f k r = DatagridResult
  { _grid_selection :: Dynamic t (Set k)
  , _grid_navigate  :: Event t URI
  , _grid_window    :: Dynamic t ViewWindow
  , _grid_columns   :: Dynamic t [a]
  , _grid_value     :: Dynamic t (MapSubset Int r)
  , _grid_filter    :: Dynamic t f
  }

data Column t m a f k r = Column
  { _column_label   :: Text
  , _column_viewer  :: Dynamic t r -> m ()
  , _column_orderBy :: (k, SortOrder -> a)
  , _column_filterBy
      :: (  [FilterCondition]
      ,  f -> FilterCondition
      ,  f
      -> FilterCondition
      -> Event t FilterCondition
      -> m (Dynamic t (f -> f))
      )
  }

data ViewWindow = ViewWindow
  { _win_offset :: Integer
  , _win_limit  :: Integer
  }

data DatagridConfig t m a f k0 k r = DatagridConfig
  { _gridConfig_columns       :: [Column t m a f k0 r]
  , _gridConfig_selectAll     :: Event t Bool
  , _gridConfig_setValue      :: Event t (MapSubset Int (Maybe r))
  , _gridConfig_toLink        :: r -> L.Link
  , _gridConfig_toPrimary     :: r -> k
  , _gridConfig_initialWindow :: ViewWindow
  , _gridConfig_initialSort   :: Map k0 SortOrder
  , _gridConfig_initialFilter :: f
  }

datagridDyn
  :: ( Ord k
     , Ord k0
     , DomBuilder t m
     , MonadHold t m
     , PostBuild t m
     , MonadFix m
     , MonadIO m
     , Prerender js t m
     )
  => DatagridConfig t m a f k0 k r
  -> m (DatagridResult t a f k r)
datagridDyn cfg = datagrid 2 $ \dynHeight -> do
  rec
    (iSelectAllEv, colheads, qfs) <- el "thead" $ do
      (selectAllEv', colheads') <- headMultiSelect $ do
        el "th" blank
        Map.traverseWithKey
          (\k c -> columnHead' ((k `Set.member`) <$> colKeysDyn) $ do
            dynSort <- sortlabel
              (constDyn (_column_label c))
              (_gridConfig_initialSort cfg Map.!? fst (_column_orderBy c))
              never

            pure (fmap (snd (_column_orderBy c)) <$> dynSort)
          )
          colsIndexed
      el "tr" $ do
        el "td" blank
        el "td" blank
        qfs' <- Map.traverseWithKey
          (\k c ->
            let (avail, initC, editor) = _column_filterBy c
                initF                  = _gridConfig_initialFilter cfg
            in  elDynClass "td"
                           (visibleDynClass . (k `Set.member`) <$> colKeysDyn)
                  $ withFilterCondition avail (initC initF) (editor initF)
          )
          colsIndexed
        pure (selectAllEv', colheads', qfs')
    let selectAllEv = leftmost [iSelectAllEv, _gridConfig_selectAll cfg]

    totalCountWithDef <- holdDyn 0 (mkTotalCount <$> _gridConfig_setValue cfg)
    let heightDynWithDef =
          maybe
              (36
              * fromIntegral (_win_limit $ _gridConfig_initialWindow cfg)
              `Prelude.div` bufferSize
              )
              (\x -> Prelude.round x - 107)
            <$> dynHeight

    (dynWindow, dynR) <-
      elDynAttr "tbody" (mkHeightAttr <$> dynHeight)
      $ virtualListBuffered'
          bufferSize
          heightDynWithDef
          36
          totalCountWithDef
          (fromIntegral $ initOffset $ _gridConfig_initialWindow cfg)
          never
          Prelude.id
          Map.empty
          (_ms_data <$> _gridConfig_setValue cfg)
      $ \attrs _ initR updateR -> do
          rowMultiSelect
              attrs
              False
              (leftmost
                [ selectAllEv
                , attachWith isSelected (current selectionDyn) updateR
                ]
              )
            $ do
                dynLnk <- holdDyn (_gridConfig_toLink cfg initR)
                                  (_gridConfig_toLink cfg <$> updateR)
                lnkEv <- safelinkCell dynLnk angleDoubleRightIcon
                rDyn  <- holdDyn initR updateR
                rcols <- Reflex.Dom.list
                  dynCols
                  (\dynF -> el "td" $ do
                    {-
                dynEv <-
                  dyn
                    (   (\f -> getCompose (_column_editor f initR updateR))
                    <$> dynF
                    )
                -}
                    dyn_ ((`_column_viewer` rDyn) <$> dynF)
                    let dynEv = never
                    join <$> holdDyn (constDyn Prelude.id) dynEv
                  )
                let r = foldResult rDyn rcols
                pure (lnkEv, r)

    let selectionDyn = mkSelectionDyn dynR
    let selcountDyn  = Set.size <$> selectionDyn

    totalCountDyn <- holdDyn Nothing
                             (_ms_totalCount <$> _gridConfig_setValue cfg)

    colKeysDyn <- el "tfoot" $ tfooter $ do
      selectedCountInfo selcountDyn
      colKeysDyn' <- showHideColumns $ fmap _column_label colsIndexed

      elClass "div" "pagination" $ el "span" $ dynText
        (resultSummary' <$> totalCountDyn)

      pure colKeysDyn'

    let dynCols = Map.restrictKeys colsIndexed <$> colKeysDyn

    let dynRSelected = joinDynThroughMap
          (Map.map (\(x, (_, y)) -> (,) <$> x <*> y) <$> dynR)

    let pageDyn = mkWindow <$> dynWindow

  pure $ DatagridResult
    { _grid_navigate  = switchDyn
                          (leftmost . fmap (fst . snd) . Map.elems <$> dynR)
    , _grid_window    = pageDyn
    , _grid_selection = selectionDyn
    , _grid_columns   = catMaybes . Map.elems <$> joinDynThroughMap
                          (constDyn colheads)
    , _grid_value = MapSubset <$> (fmap snd <$> dynRSelected) <*> totalCountDyn
    , _grid_filter    = foldResult (constDyn $ _gridConfig_initialFilter cfg)
                                   (constDyn qfs)
    }

 where
  initOffset w
    | _win_offset w == 0
    = 0
    | otherwise
    = _win_offset w
      +             (fromIntegral bufferSize - 1)
      *             _win_limit w
      `Prelude.div` (2 * fromIntegral bufferSize)

  bufferSize = 2

  resultSummary' (Just i') = pack (show i') <> " results"
  resultSummary' Nothing   = ""

  mkWindow (off, lim) =
    ViewWindow { _win_offset = fromIntegral off, _win_limit = fromIntegral lim }

  mkTotalCount (MapSubset _  (Just x)) = fromIntegral x
  mkTotalCount (MapSubset xs _       ) = Map.size xs

  mkHeightAttr Nothing =
    Map.singleton "style" "position:relative;height:calc(100vh - 17rem - 1px)"
  mkHeightAttr (Just x) = Map.singleton
    "style"
    (  "position:relative;height:"
    <> pack (show (Prelude.round x - (107 :: Int)))
    <> "px"
    )
  colsIndexed = Map.fromList $ zip [0 ..] $ _gridConfig_columns cfg

  foldResult
    :: Reflex t
    => Dynamic t a
    -> Dynamic t (Map Integer (Dynamic t (a -> a)))
    -> Dynamic t a
  foldResult x xs = foldr ($) <$> x <*> joinDynThroughMap xs

  mkSelectionDyn =
    fmap (Set.fromList . fmap fst . Map.elems . Map.filter snd)
      . joinDynThroughMap
      . fmap (fmap (\(selDyn, (_, x)) -> primaryKeyWithSel <$> x <*> selDyn))

  primaryKeyWithSel r s' = (_gridConfig_toPrimary cfg r, s')

  isSelected selset r = _gridConfig_toPrimary cfg r `Set.member` selset

addDeletes :: Ord k => Set k -> MapSubset k a -> MapSubset k (Maybe a)
addDeletes oldSet m = m { _ms_data = diffMapSet oldSet (_ms_data m) }

diffMapSet :: (Ord k) => Set k -> Map k v -> Map k (Maybe v)
diffMapSet olds news = fmap Just news
  `Map.union` Map.fromSet
                (const Nothing)
                (Set.difference olds (Map.keysSet news))


-- | Modified from Reflex.Dom.Widget.Lazy for better fit with datagrid
virtualList'
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Ord k
     , Prerender js t m
     )
  => Dynamic t Int -- ^ A 'Dynamic' of the visible region's height in pixels
  -> Int -- ^ The fixed height of each row in pixels
  -> Dynamic t Int -- ^ A 'Dynamic' of the total number of items
  -> Int -- ^ The index of the row to scroll to on initialization
  -> Event t Int -- ^ An 'Event' containing a row index. Used to scroll to the given index.
  -> (k -> Int) -- ^ Key to Index function, used to position items.
  -> Map k v -- ^ The initial 'Map' of items
  -> Event t (Map k (Maybe v)) -- ^ The update 'Event'. Nothing values are removed from the list and Just values are added or updated.
  -> (Map Text Text -> k -> v -> Event t v -> m a) -- ^ The row child element builder.
  -> m (Dynamic t (Int, Int), Dynamic t (Map k a)) -- ^ A tuple containing: a 'Dynamic' of the index (based on the current scroll position) and number of items currently being rendered, and the 'Dynamic' list result
virtualList' heightPx rowPx maxIndex i0 setI keyToIndex items0 itemsUpdate itemBuilder
  = do
    let id'           = "datagrid-viewport" -- Should be unique per virtuallist
        virtualH      = mkVirtualHeight <$> maxIndex
        viewportStyle = fmap (mkViewport id') heightPx
    pb <- getPostBuild
    rec (viewport, result) <-
          elDynAttr' "div" viewportStyle
          $ elDynAttr "div" virtualH
          $ listWithKeyShallowDiff items0 itemsUpdate
          $ \k v e -> itemBuilder (mkRow k) k v e
        scrollPosition <- holdDyn 0 $ leftmost
          [ Prelude.round <$> domEvent Scroll viewport
          , fmap (const (i0 * rowPx)) pb
          ]
        let window = zipDynWith (findWindow rowPx) heightPx scrollPosition
    prerender_ (pure ()) $ do
      pb' <- delay 5 pb
      performEvent_ $ ffor (leftmost [setI, i0 <$ pb']) $ \i' -> liftJSM $ do
        doc        <- jsg ("document" :: Text)
        viewportEl <- doc ^. js1 ("getElementById" :: Text) (id' :: Text)
        viewportEl ^. jss ("scrollTop" :: Text) (i' * rowPx)

    uniqWindow <- holdUniqDyn window
    return (uniqWindow, result)
 where
  toStyleAttr m = Map.singleton "style"
    $ Map.foldrWithKey (\k v kv -> k <> ":" <> v <> ";" <> kv) "" m
  mkViewport id' h = Map.singleton "id" id' <> toStyleAttr
    (Map.fromList
      [ ("overflow", "auto")
      , ("position", "absolute")
      , ("left"    , "0")
      , ("right"   , "0")
      , ("height"  , pack (show h) <> "px")
      ]
    )
  mkVirtualHeight h =
    let h' = h * rowPx
    in  toStyleAttr $ Map.fromList
          [ ("height"  , pack (show h') <> "px")
          , ("overflow", "hidden")
          , ("position", "relative")
          ]
  mkRow k = toStyleAttr $ Map.fromList
    [ ("height"  , pack (show rowPx) <> "px")
    , ("top", (<> "px") (pack $ show $ keyToIndex k * rowPx))
    , ("position", "absolute")
    ]
  findWindow sizeIncrement windowSize startingPosition =
    let (startingIndex, _) = startingPosition `divMod'` sizeIncrement
        numItems = (windowSize + sizeIncrement - 1) `Prelude.div` sizeIncrement
    in  (startingIndex, numItems)

-- | Modified from Reflex.Dom.Widget.Lazy for better fit with datagrid
virtualListBuffered'
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , Prerender js t m
     , MonadFix m
     , Ord k
     )
  => Int
  -> Dynamic t Int
  -> Int
  -> Dynamic t Int
  -> Int
  -> Event t Int
  -> (k -> Int)
  -> Map k v
  -> Event t (Map k (Maybe v))
  -> (Map Text Text -> k -> v -> Event t v -> m a)
  -> m
       ( Dynamic t (Int, Int)
       , Dynamic t (Map k a)
       )
virtualListBuffered' buffer heightPx rowPx maxIndex i0 setI keyToIndex items0 itemsUpdate itemBuilder
  = do
    (win, m) <- virtualList' heightPx
                             rowPx
                             maxIndex
                             i0
                             setI
                             keyToIndex
                             items0
                             itemsUpdate
                             itemBuilder
    pb <- getPostBuild
    let extendWin o l =
          (Prelude.max 0 (o - l * (buffer - 1) `Prelude.div` 2), l * buffer)
    rec
      let winHitEdge = attachWithMaybe
            (\(oldOffset, oldLimit) (winOffset, winLimit) ->
              if winOffset
                 >  oldOffset
                 && winOffset
                 +  winLimit
                 <  oldOffset
                 +  oldLimit
              then
                Nothing
              else
                Just (extendWin winOffset winLimit)
            )
            (current winBuffered)
            (updated win)
      winBuffered <-
        holdDyn (0, 0)
          $ leftmost
              [ winHitEdge
              , attachPromptlyDynWith (\(x, y) _ -> extendWin x y) win pb
              ]
    return (winBuffered, m)

