{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.MMark
  ( renderDom
  )
where

import qualified Control.Foldl                 as Fold
import           Control.Monad.Fix              ( MonadFix )
import           Data.Bool                      ( bool )
import           Data.Dependent.Sum
import           Data.Foldable                  ( toList )
import           Data.Functor.Compose           ( Compose(..) )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.GADT.Compare
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Type.Equality             ( (:~:)(..) )
-- import qualified GHC.SyntaxHighlighter         as GS
import           Reflex.Dom              hiding ( Link )
import qualified Skylighting                   as S
import           Text.MMark                     ( MMark )
import qualified Text.MMark                    as MMark
import           Text.MMark.Extension
import           Text.URI                       ( URI )
import qualified Text.URI                      as URI


renderDom
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t MMark
  -> m ()
renderDom dynx = renderBlocks (markToDBlocks <$> dynx)

markToDBlocks :: MMark -> [DBlock]
markToDBlocks y =
  MMark.runScanner y (Fold.Fold (\x a -> x . (mmarkBlockDSum a :)) id ($ []))

renderBlocks
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t [DBlock]
  -> m ()
renderBlocks xs = do
  _ <- simpleList xs $ \x -> do
    x' <- factorDyn x
    dyn $ fmap renderBlock x'
  pure ()

renderBlock
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => DynBlock t
  -> m ()
renderBlock = \case
  ThematicBreakTag :=> _         -> el "hr" blank
  Heading1Tag      :=> Compose h -> el "h1" $ renderInlines (runIdentity <$> h)
  Heading2Tag      :=> Compose h -> el "h2" $ renderInlines (runIdentity <$> h)
  Heading3Tag      :=> Compose h -> el "h3" $ renderInlines (runIdentity <$> h)
  Heading4Tag      :=> Compose h -> el "h4" $ renderInlines (runIdentity <$> h)
  Heading5Tag      :=> Compose h -> el "h5" $ renderInlines (runIdentity <$> h)
  Heading6Tag      :=> Compose h -> el "h6" $ renderInlines (runIdentity <$> h)
  CodeBlockTag     :=> Compose d -> do
    let (infoString, txt) = splitDynPure (runIdentity <$> d)
    _ <- listWithKey (Map.singleton <$> infoString <*> txt) renderCode
    pure ()
  NakedTag :=> Compose html -> renderInlines (runIdentity <$> html)
  ParagraphTag :=> Compose html ->
    el "p" $ renderInlines (runIdentity <$> html)
  BlockquoteTag :=> Compose blocks -> do
    el "blockquote" $ renderBlocks (runIdentity <$> blocks)
  OrderedListTag :=> Compose d ->
    let (i, items) = splitDynPure (runIdentity <$> d)
    in  do
          _ <- elDynAttr "ol" (fmap startFrom i) $ do
            simpleList (NE.toList <$> items) $ \x -> do
              el "li" $ renderBlocks x
          pure ()
  UnorderedListTag :=> Compose items -> do
    _ <- el "ul" $ simpleList (NE.toList . runIdentity <$> items) $ \x ->
      el "li" $ renderBlocks x
    pure ()
  TableTag :=> Compose d ->
    let (calign, allrows) = splitDynPure (runIdentity <$> d)
        (hs    , rows   ) = splitDynPure (toPair <$> allrows)
    in  el "table" $ do
          _ <- el "thead" $ do
            el "tr" $ simpleList (zipList <$> calign <*> hs) $ \v ->
              let (a, h) = splitDynPure v
              in  elDynAttr "th" (alignStyle <$> a) $ renderInlines h
          _ <- el "tbody" $ do
            simpleList rows $ \row -> do
              el "tr" $ simpleList (zipList <$> calign <*> row) $ \v ->
                let (a, h) = splitDynPure v
                in  elDynAttr "td" (alignStyle <$> a) $ renderInlines h
          pure ()
 where
  zipList x y = NE.toList (NE.zip x y)
  toPair (x :| xs) = (x, xs)
  startFrom :: Word -> Map Text Text
  startFrom idx = bool mempty ("start" =: Text.pack (show idx)) (idx /= 1)

  alignStyle :: CellAlign -> Map Text Text
  alignStyle = \case
    CellAlignDefault -> mempty
    CellAlignLeft    -> Map.singleton "style" "text-align:left"
    CellAlignRight   -> Map.singleton "style" "text-align:right"
    CellAlignCenter  -> Map.singleton "style" "text-align:center"

renderInlines
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Foldable f)
  => Dynamic t (f DInline)
  -> m ()
renderInlines xs = do
  _ <- simpleList (toList <$> xs) $ \x -> do
    x' <- factorDyn x
    dyn $ fmap renderInline x'
  pure ()

renderInline
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => DynInline t
  -> m ()
renderInline = \case
  PlainTag     :=> Compose txt -> dynText (runIdentity <$> txt)
  LineBreakTag :=> Compose _   -> el "br" blank
  EmphasisTag :=> Compose inner ->
    el "em" $ renderInlines (runIdentity <$> inner)
  StrongTag :=> Compose inner ->
    el "strong" $ renderInlines (runIdentity <$> inner)
  StrikeoutTag :=> Compose inner ->
    el "strike" $ renderInlines (runIdentity <$> inner)
  SubscriptTag :=> Compose inner ->
    el "sub" $ renderInlines (runIdentity <$> inner)
  SuperscriptTag :=> Compose inner ->
    el "sup" $ renderInlines (runIdentity <$> inner)
  CodeSpanTag :=> Compose dynTxt -> do
    let ch = '$'
        splitDynText =
          (\txt ->
              if Text.length txt
                   >= 2
                   && Text.head txt
                   == ch
                   && Text.last txt
                   == ch
                then (Left . Text.dropEnd 1 . Text.drop 1) txt
                else Right txt
            )
            .   runIdentity
            <$> dynTxt
        dynCode = el "code" . dynText
        dynMath x = elClass "span" "math inline" $ do
          text "\\("
          dynText x
          text "\\)"
    splitDynText' <- eitherDyn splitDynText
    _             <- dyn (either dynMath dynCode <$> splitDynText')
    pure ()
  LinkTag :=> Compose d ->
    let (inner, dest, mtitle) = splitDyn3Pure (runIdentity <$> d)
        dynTitle              = maybe mempty (Map.singleton "title") <$> mtitle
        dynHref               = Map.singleton "href" . URI.render <$> dest
    in  elDynAttr "a" (dynHref <> dynTitle) $ renderInlines inner
  ImageTag :=> Compose d ->
    let (desc, src, mtitle) = splitDyn3Pure (runIdentity <$> d)
        dynTitle            = maybe mempty (Map.singleton "title") <$> mtitle
        dynAlt              = Map.singleton "alt" . asPlainText <$> desc
        dynSrc              = Map.singleton "src" . URI.render <$> src
    in  elDynAttr "img" (dynAlt <> dynSrc <> dynTitle) blank

splitDyn3Pure
  :: Reflex t => Dynamic t (a, b, c) -> (Dynamic t a, Dynamic t b, Dynamic t c)
splitDyn3Pure dynxs =
  ( (\(x, _, _) -> x) <$> dynxs
  , (\(_, x, _) -> x) <$> dynxs
  , (\(_, _, x) -> x) <$> dynxs
  )

type DBlock = DSum BlockTag Identity
type DynBlock t = DSum BlockTag (Compose (Dynamic t) Identity)

-- | `DSum` variant of `MMark.Block`
data BlockTag a where
  -- | Thematic break, leaf block
  ThematicBreakTag ::BlockTag ()
  -- | Heading (level 1), leaf block
  Heading1Tag ::BlockTag (NonEmpty DInline)
  -- | Heading (level 2), leaf block
  Heading2Tag ::BlockTag (NonEmpty DInline)
  -- | Heading (level 3), leaf block
  Heading3Tag ::BlockTag (NonEmpty DInline)
  -- | Heading (level 4), leaf block
  Heading4Tag ::BlockTag (NonEmpty DInline)
  -- | Heading (level 5), leaf block
  Heading5Tag ::BlockTag (NonEmpty DInline)
  -- | Heading (level 6), leaf block
  Heading6Tag ::BlockTag (NonEmpty DInline)
  -- | Code block, leaf block with info string and contents
  CodeBlockTag ::BlockTag (Maybe Text, Text)
  -- | Naked content, without an enclosing tag
  NakedTag ::BlockTag (NonEmpty DInline)
  -- | Paragraph, leaf block
  ParagraphTag ::BlockTag (NonEmpty DInline)
  -- | Blockquote container block
  BlockquoteTag ::BlockTag [DBlock]
  -- | Ordered list ('Word' is the start index), container block
  OrderedListTag ::BlockTag (Word, NonEmpty [DBlock])
  -- | Unordered list, container block
  UnorderedListTag ::BlockTag (NonEmpty [DBlock])
  -- | Table, first argument is the alignment options, then we have a
  -- 'NonEmpty' list of rows, where every row is a 'NonEmpty' list of
  -- cells, where every cell is an @a@ thing.
  --
  -- The first row is always the header row, because pipe-tables that we
  -- support cannot lack a header row.
  TableTag ::BlockTag (NonEmpty CellAlign, NonEmpty (NonEmpty (NonEmpty DInline)))

mmarkBlockDSum :: Bni -> DBlock
mmarkBlockDSum = \case
  ThematicBreak    -> ThematicBreakTag ==> ()
  Heading1 xs      -> Heading1Tag ==> fmap mmarkInlineDSum xs
  Heading2 xs      -> Heading2Tag ==> fmap mmarkInlineDSum xs
  Heading3 xs      -> Heading3Tag ==> fmap mmarkInlineDSum xs
  Heading4 xs      -> Heading4Tag ==> fmap mmarkInlineDSum xs
  Heading5 xs      -> Heading5Tag ==> fmap mmarkInlineDSum xs
  Heading6 xs      -> Heading6Tag ==> fmap mmarkInlineDSum xs
  CodeBlock x y    -> CodeBlockTag ==> (x, y)
  Naked      xs    -> NakedTag ==> fmap mmarkInlineDSum xs
  Paragraph  xs    -> ParagraphTag ==> fmap mmarkInlineDSum xs
  Blockquote xs    -> BlockquoteTag ==> fmap mmarkBlockDSum xs
  OrderedList x y  -> OrderedListTag ==> (x, fmap (fmap mmarkBlockDSum) y)
  UnorderedList xs -> UnorderedListTag ==> fmap (fmap mmarkBlockDSum) xs
  Table x ys       -> TableTag ==> (x, fmap (fmap (fmap mmarkInlineDSum)) ys)

instance GEq BlockTag where
  geq ThematicBreakTag ThematicBreakTag = Just Refl
  geq Heading1Tag      Heading1Tag      = Just Refl
  geq Heading2Tag      Heading2Tag      = Just Refl
  geq Heading3Tag      Heading3Tag      = Just Refl
  geq Heading4Tag      Heading4Tag      = Just Refl
  geq Heading5Tag      Heading5Tag      = Just Refl
  geq Heading6Tag      Heading6Tag      = Just Refl
  geq CodeBlockTag     CodeBlockTag     = Just Refl
  geq NakedTag         NakedTag         = Just Refl
  geq ParagraphTag     ParagraphTag     = Just Refl
  geq BlockquoteTag    BlockquoteTag    = Just Refl
  geq OrderedListTag   OrderedListTag   = Just Refl
  geq UnorderedListTag UnorderedListTag = Just Refl
  geq TableTag         TableTag         = Just Refl
  geq _                _                = Nothing


type DInline = DSum InlineTag Identity
type DynInline t = DSum InlineTag (Compose (Dynamic t) Identity)

-- | `DSum` variant of `MMark.Inline`
data InlineTag a where
  -- | Plain text
  PlainTag ::InlineTag Text
  -- | Line break (hard)
  LineBreakTag ::InlineTag ()
  -- | Emphasis
  EmphasisTag ::InlineTag (NonEmpty DInline)
  -- | Strong emphasis
  StrongTag ::InlineTag (NonEmpty DInline)
  -- | Strikeout
  StrikeoutTag ::InlineTag (NonEmpty DInline)
  -- | Subscript
  SubscriptTag ::InlineTag (NonEmpty DInline)
  -- | Superscript
  SuperscriptTag ::InlineTag (NonEmpty DInline)
  -- | Code span
  CodeSpanTag ::InlineTag Text
  -- | Link with text, destination, and optionally title
  LinkTag ::InlineTag (NonEmpty DInline, URI, Maybe Text)
  -- | Image with description, URL, and optionally title
  ImageTag ::InlineTag (NonEmpty Inline, URI, Maybe Text)


mmarkInlineDSum :: Inline -> DInline
mmarkInlineDSum = \case
  Plain x        -> PlainTag ==> x
  LineBreak      -> LineBreakTag ==> ()
  Emphasis    xs -> EmphasisTag ==> fmap mmarkInlineDSum xs
  Strong      xs -> StrongTag ==> fmap mmarkInlineDSum xs
  Strikeout   xs -> StrikeoutTag ==> fmap mmarkInlineDSum xs
  Subscript   xs -> SubscriptTag ==> fmap mmarkInlineDSum xs
  Superscript xs -> SuperscriptTag ==> fmap mmarkInlineDSum xs
  CodeSpan    x  -> CodeSpanTag ==> x
  Link  xs y z   -> LinkTag ==> (fmap mmarkInlineDSum xs, y, z)
  Image xs y z   -> ImageTag ==> (xs, y, z)

instance GEq InlineTag where
  geq PlainTag       PlainTag       = Just Refl
  geq LineBreakTag   LineBreakTag   = Just Refl
  geq EmphasisTag    EmphasisTag    = Just Refl
  geq StrongTag      StrongTag      = Just Refl
  geq StrikeoutTag   StrikeoutTag   = Just Refl
  geq SubscriptTag   SubscriptTag   = Just Refl
  geq SuperscriptTag SuperscriptTag = Just Refl
  geq CodeSpanTag    CodeSpanTag    = Just Refl
  geq LinkTag        LinkTag        = Just Refl
  geq ImageTag       ImageTag       = Just Refl
  geq _              _              = Nothing


renderCode
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Maybe Text
  -> Dynamic t Text
  -> m ()
renderCode (Just "mathjax") code = el "p" $ do
  _ <- simpleList (Text.lines <$> code) $ \txt ->
    elClass "span" "math display" $ do
      text "\\["
      dynText txt
      text "\\]"
  pure ()

{-
renderCode (Just "haskell") code = codeEl (Just "haskell") $ do
  dynTok <- maybeDyn (GS.tokenizeHaskell <$> code)
  dyn_ (maybe (dynText code) renderGhc <$> dynTok)
 where
  renderGhc ts = do
    _ <- simpleList ts $ \t ->
      let (tokType, tok) = splitDynPure t
      in  elDynClass "span" (tokenClassGhc <$> tokType) $ dynText tok
    pure ()
-}

renderCode (Just lang) code = codeEl (Just lang) $ do
  dynTok <- maybeDyn (tokens <$> code)
  dyn_ (maybe (dynText code) renderSkylighting <$> dynTok)
 where
  renderSkylighting tss = do
    _ <- simpleList tss $ \ts -> do
      _ <- simpleList ts $ \t ->
        let (tokType, tok) = splitDynPure t
        in  elDynClass "span" (tokenClass <$> tokType) $ dynText tok
      text "\n"
    pure ()

  tokens s = do
    syntax <- S.lookupSyntax lang S.defaultSyntaxMap
    either (const Nothing) pure $ S.tokenize tokenizerConfig syntax s

  tokenizerConfig = S.TokenizerConfig { S.syntaxMap   = S.defaultSyntaxMap
                                      , S.traceOutput = False
                                      }


renderCode Nothing code = codeEl Nothing $ dynText code

codeEl :: (DomBuilder t m) => Maybe Text -> m a -> m a
codeEl (Just lang) x = elClass "div" "source-code" $ el "pre" $ elClass
  "code"
  ("language-" <> lang)
  x
codeEl Nothing x = elClass "div" "source-code" $ el "pre" $ el "code" x

tokenClass :: S.TokenType -> Text
tokenClass = \case
  S.KeywordTok        -> "kw"
  S.DataTypeTok       -> "dt"
  S.DecValTok         -> "dv"
  S.BaseNTok          -> "bn"
  S.FloatTok          -> "fl"
  S.CharTok           -> "ch"
  S.StringTok         -> "st"
  S.CommentTok        -> "co"
  S.OtherTok          -> "ot"
  S.AlertTok          -> "al"
  S.FunctionTok       -> "fu"
  S.RegionMarkerTok   -> "re"
  S.ErrorTok          -> "er"
  S.ConstantTok       -> "cn"
  S.SpecialCharTok    -> "sc"
  S.VerbatimStringTok -> "vs"
  S.SpecialStringTok  -> "ss"
  S.ImportTok         -> "im"
  S.DocumentationTok  -> "do"
  S.AnnotationTok     -> "an"
  S.CommentVarTok     -> "cv"
  S.VariableTok       -> "va"
  S.ControlFlowTok    -> "cf"
  S.OperatorTok       -> "op"
  S.BuiltInTok        -> "bu"
  S.ExtensionTok      -> "ex"
  S.PreprocessorTok   -> "pp"
  S.AttributeTok      -> "at"
  S.InformationTok    -> "in"
  S.WarningTok        -> "wa"
  S.NormalTok         -> ""

{-
tokenClassGhc :: GS.Token -> Text
tokenClassGhc = \case
  GS.KeywordTok     -> "kw"
  GS.PragmaTok      -> "pr"
  GS.SymbolTok      -> "sy"
  GS.VariableTok    -> ""
  GS.ConstructorTok -> "cr"
  GS.OperatorTok    -> "op"
  GS.CharTok        -> "ch"
  GS.StringTok      -> "st"
  GS.IntegerTok     -> "it"
  GS.RationalTok    -> "ra"
  GS.CommentTok     -> "co"
  GS.SpaceTok       -> ""
  GS.OtherTok       -> "ot"
-}
