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
  , formProp
  , gridProp
  , Property(..)
  ) where

import           Clay                    hiding ( icon )
import           Control.Lens            hiding ( (#)
                                                , none
                                                )
import           Control.Monad                  ( join )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Default
import           Data.Functor.Compose
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Sum(..) )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text
                                                , pack
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
    Clay.button ? do
      important
        $ margin (rem (-1 / 4)) (rem (1 / 4)) (rem (-1 / 4)) (rem (1 / 4))
      paddingAll (rem (1 / 4))

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

safelinkCell
  :: (DomBuilder t m, PostBuild t m) => L.Link -> m () -> m (Event t URI)
safelinkCell lnk cnt = do
  clickEv <- elClass "td" "center"
    $ ahrefPreventDefault ("/" <> toUrlPiece lnk) (constDyn False) cnt
  pure $ coerceUri (L.linkURI lnk) <$ clickEv

angleDoubleRightIcon :: (DomBuilder t m, PostBuild t m) => m ()
angleDoubleRightIcon =
  icon def { _iconConfig_direction = constDyn DirRight } angleDoubleIcon

data SortOrder = Descending | Ascending deriving (Eq, Show)

sortlabel
  :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -> m (Dynamic t (Maybe SortOrder))
sortlabel dynLbl = do
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

data Page = Page
  { _page_num  :: Int
  , _page_size :: Int
  }
  deriving (Eq, Show)

data PageSize = Page10 | Page20 | Page50 | Page100 deriving (Eq, Show, Enum, Bounded, Ord)

instance Default PageSize where
  def = Page10

printPageSize :: DomBuilder t m => PageSize -> m ()
printPageSize = text . pack . show . pageSize

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
  => Dynamic t (Maybe Int)
  -> m (Dynamic t Page)
paginationInput totalResults = elClass "div" "pagination" $ do
  el "span" $ text "Results per page"
  dynLim <- _inputEl_value
    <$> selectInput (inputConfig' (OpElem printPageSize) (Just Page10))
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
  => m a
  -> m (Dynamic t FilterCondition, a)
withFilterCondition editor = inputGroup def $ do
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
              [minBound .. maxBound]

      dynVal <- holdDyn def selectEv

  x <- editor

  pure (dynVal, x)

headMultiSelect :: (DomBuilder t m) => m a -> m (Event t Bool, a)
headMultiSelect theadTr = el "tr" $ do
  selectAllDyn <- el "th" $ checkboxInputSimple False never mempty
  x            <- theadTr
  pure (updated selectAllDyn, x)



data Property a c t m b = Property
  { _prop_editor      :: InputConfig' c t m b -> m (DomInputEl t m b)
  , _prop_extraConfig :: c b
  , _prop_label       :: Text
  , _prop_lens        :: Lens' a b
  }

-- | Converts an editor into an editor which accounts for focus
--
-- 1. If the user is focussed, no external set value events are applied
-- 2. The output dynamic is only updated on lose focus
respectFocus
  :: (DomBuilder t m, MonadFix m)
  => (InputConfig' c t m b -> m (DomInputEl t m b))
  -> InputConfig' c t m b
  -> m (DomInputEl t m b)
respectFocus editor cfg = do
  rec r <- editor cfg
        { _inputConfig_setValue = gate
                                    (current
                                      (Prelude.not <$> _inputEl_hasFocus r)
                                    )
                                    (_inputConfig_setValue cfg)
        }

  pure r

formProp
  :: (DomBuilder t m, PostBuild t m, MonadIO m)
  => Property a c t m b
  -> a
  -> Event t a
  -> Compose m (Dynamic t) (a -> a)
formProp prp initVal update =
  Compose $ fmap (set (_prop_lens prp)) . _inputEl_value <$> labeled
    (_prop_label prp)
    (_prop_editor prp)
    (propInputConfig prp initVal update)

propInputConfig
  :: (DomBuilder t m)
  => Property a c t m b
  -> a
  -> Event t a
  -> InputConfig' c t m b
propInputConfig prp initVal update =
  (inputConfig' (_prop_extraConfig prp) (view (_prop_lens prp) initVal))
    { _inputConfig_setValue = view (_prop_lens prp) <$> update
    }

gridProp
  :: (DomBuilder t m, MonadFix m)
  => Property a c t m b
  -> (Text, k -> a -> Event t a -> Compose m (Dynamic t) (a -> a))
gridProp prp =
  ( _prop_label prp
  , \_ initVal update ->
    Compose $ fmap (set (_prop_lens prp)) . _inputEl_value <$> respectFocus
      (_prop_editor prp)
      (propInputConfig prp initVal update)
  )


datagridDyn
  :: ( Ord k
     , DomBuilder t m
     , MonadHold t m
     , PostBuild t m
     , MonadFix m
     , MonadIO m
     )
  => (k -> L.Link)
  -> [(Text, k -> r -> Event t r -> Compose m (Dynamic t) (r -> r))]
  -> Map k r
  -> Event t (Map k (Maybe r))
  -> m (Event t URI)
datagridDyn toLnk cols' initRows updateRows = datagrid 2 $ do
  rec
    selectAllEv <- el "thead" $ do
      (selectAllEv', _) <- headMultiSelect $ do
        el "th" blank
        Reflex.Dom.list
          dynCols
          (\c -> columnHead $ do
            _ <- sortlabel (fst <$> c)
            filterEl BottomRight (constDyn True) blank
          )
      el "tr" $ do
        el "td" blank
        el "td" blank
        _ <- Reflex.Dom.list
          dynCols
          (\_ ->
            el "td" $ withFilterCondition $ Components.Input.Basic.textInput
              (inputConfig "")
          )
        pure selectAllEv'

    dynR <-
      elAttr "tbody" ("style" =: "height:20rem")
      $ listWithKeyShallowDiff initRows updateRows
      $ \k initR updateR -> do
          rowMultiSelect False selectAllEv $ do
            lnkEv <- safelinkCell (toLnk k) angleDoubleRightIcon
            rcols <- Reflex.Dom.list
              dynCols
              (\dynF -> el "td" $ do
                dynEv <- dyn
                  ((\(_, f) -> getCompose (f k initR updateR)) <$> dynF)
                join <$> holdDyn (constDyn Prelude.id) dynEv
              )
            let r = foldResult initR rcols
            pure (lnkEv, r)

    let selcountDyn = (countSelected . Map.elems) =<< dynR

    colKeysDyn <- el "tfoot" $ tfooter $ do
      selectedCountInfo selcountDyn
      colKeysDyn' <- showHideColumns $ fmap fst colsIndexed

      _           <- paginationInput (constDyn (Just 51))
      pure colKeysDyn'

    let dynCols = Map.restrictKeys colsIndexed <$> colKeysDyn

  pure $ switchDyn (leftmost . fmap (fst . snd) . Map.elems <$> dynR)

 where
  colsIndexed = Map.fromList $ zip [0 ..] cols'

  foldResult
    :: Reflex t
    => a
    -> Dynamic t (Map Integer (Dynamic t (a -> a)))
    -> Dynamic t a
  foldResult x xs = foldr ($) x <$> joinDynThroughMap xs

  countSelected :: Reflex t => [(Dynamic t Bool, b)] -> Dynamic t Int
  countSelected = fmap getSum . mconcat . fmap (fmap (Sum . boolToNum) . fst)

  boolToNum True  = 1
  boolToNum False = 0
