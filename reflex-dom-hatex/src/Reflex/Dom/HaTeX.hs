{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module Reflex.Dom.HaTeX
  ( mainWithHead
  , writeTex
  , elLaTeX
  , elLaTeXM
  , module Text.LaTeX
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , runReaderT
                                                , join
                                                )
import           Data.Dependent.Sum
import           Data.Functor.Compose           ( Compose(..) )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.GADT.Compare
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Type.Equality             ( (:~:)(..) )
import           Language.Javascript.JSaddle
import           GHCJS.DOM                      ( currentWindowUnchecked )
import           GHCJS.DOM.WindowOrWorkerGlobalScope
                                                ( setTimeout_ )
import qualified GHCJS.DOM.Types               as DOM
import           GHCJS.DOM.HTMLElement          ( HTMLElement(..)
                                                , setInnerText
                                                )
import           Text.LaTeX
import           Text.LaTeX.Base.Syntax
import qualified Text.LaTeX.Base.Pretty        as TeX
import           Reflex.Dom

mainWithHead :: IO ()
mainWithHead = do
  mainWidget $ do
    elLaTeX (constDyn $ execLaTeXM example)

writeTex :: IO ()
writeTex =
  Text.writeFile "main.tex" $ Text.pack $ TeX.prettyLaTeX (execLaTeXM example)

example :: LaTeXM ()
example = do
  documentclass [] article
  title "Reflex Dom HaTeX"
  usepackage [] "amsmath"
  usepackage [] "amssymb"
  document $ do
    maketitle
    section "Section 1"
    "The following is an experiment in displaying LaTeX using reflex-dom."
    pure ()

data Metadata
  = Title Text
  | Author Text
  deriving (Eq, Show)

data MetadataTag
 = TitleTag
 | AuthorTag
 deriving (Eq, Ord, Show)

elLaTeXM
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Prerender js t m
     )
  => Dynamic t (LaTeXM ())
  -> m ()
elLaTeXM = elLaTeX . fmap execLaTeXM

elLaTeX
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Prerender js t m
     )
  => Dynamic t LaTeX
  -> m ()
elLaTeX xs = do
  (x, y) <- runDynamicWriterT (renderLaTeXHead (fmap laTeXToDSum xs))
  y'     <- holdUniqDyn $ Map.fromList . fmap splitToTag <$> y
  runReaderT (renderLaTeX x) y'
  pure ()

 where
  splitToTag (Title  x) = (TitleTag, x)
  splitToTag (Author x) = (AuthorTag, x)

renderLaTeXHead
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , DynamicWriter t [Metadata] m
     )
  => Dynamic t (DSum LaTeXTag Identity)
  -> m (Dynamic t (DSum LaTeXTag Identity))
renderLaTeXHead x = do
  f  <- factorDyn x
  ev <- dyn (renderLaTeXFHead <$> f)
  i  <- holdDyn (constDyn $ TeXEmptyTag ==> ()) ev
  pure $ join i

renderLaTeX
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (Dynamic t (Map MetadataTag Text)) m
     , Prerender js t m
     )
  => Dynamic t (DSum LaTeXTag Identity)
  -> m ()
renderLaTeX x = do
  f <- factorDyn x
  dyn_ (renderLaTeXF <$> f)

renderLaTeXFHead
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , DynamicWriter t [Metadata] m
     )
  => DSum LaTeXTag (Compose (Dynamic t) Identity)
  -> m (Dynamic t (DSum LaTeXTag Identity))
renderLaTeXFHead = \case
  TeXCommTag :=> Compose x -> do
    renderCommandDynHead (runIdentity <$> x)
    pure (constDyn $ TeXEmptyTag ==> ())
  TeXCommSTag :=> Compose x -> do
    renderCommandDynHead ((, []) . runIdentity <$> x)
    pure (constDyn $ TeXEmptyTag ==> ())
  TeXBracesTag :=> Compose x -> renderLaTeXHead (runIdentity <$> x)
  TeXSeqTag :=> Compose xy ->
    let (x, y) = splitDynPure (runIdentity <$> xy)
    in  renderLaTeXHead x >> renderLaTeXHead y
  TeXEnvTag :=> Compose xyz -> pure $ fmap (findDocument . runIdentity) xyz
   where
    findDocument ("document", _, z) = z
    findDocument _                  = TeXEmptyTag ==> ()
  _ -> pure (constDyn $ TeXEmptyTag ==> ())-- Other commands are invalid in the preamble

renderLaTeXF
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (Dynamic t (Map MetadataTag Text)) m
     , Prerender js t m
     )
  => DSum LaTeXTag (Compose (Dynamic t) Identity)
  -> m ()
renderLaTeXF = \case
  TeXRawTag :=> Compose x -> do
    x' <- holdUniqDyn x
    dynText (runIdentity <$> x')
  TeXCommTag  :=> Compose x    -> renderCommandDyn (runIdentity <$> x)
  TeXCommSTag :=> Compose x    -> renderCommandDyn ((, []) . runIdentity <$> x)
  TeXEnvTag   :=> Compose xyz' -> renderEnvDyn (runIdentity <$> xyz')
  TeXMathTag :=> Compose xy ->
    let (x, y) = splitDynPure (runIdentity <$> xy) in renderMath x y
  TeXLineBreakTag :=> Compose _ -> el "br" blank
  TeXBracesTag    :=> Compose x -> renderLaTeX (runIdentity <$> x)
  TeXCommentTag   :=> Compose _ -> blank
  TeXSeqTag :=> Compose xy ->
    let (x, y) = splitDynPure (runIdentity <$> xy)
    in  renderLaTeX x >> renderLaTeX y
  TeXEmptyTag :=> Compose _ -> blank

renderMath
  :: (DomBuilder t m, Prerender js t m)
  => Dynamic t MathType
  -> Dynamic t LaTeX
  -> m ()
renderMath x y = prerender_ blank (renderMath' x y)

renderMath'
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadHold t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     )
  => Dynamic t MathType
  -> Dynamic t LaTeX
  -> m ()
renderMath' x y = do
  dynMath <- holdUniqDyn (render' <$> x <*> y)

  (e, _)  <- el' "div" blank

  let v = DOM.uncheckedCastTo HTMLElement $ _element_raw e

  (loadEv, raiseLoad) <- newTriggerEvent
  liftJSM $ do
    window <- currentWindowUnchecked
    rec tryInit <- function $ \_ _ _ -> do
          mathjax <- jsg ("MathJax" :: Text)
          undef   <- valIsUndefined mathjax
          if undef
            then setTimeout_ window tryInit (Just 50)
            else do
              mathjaxobj  <- valToObject mathjax
              tsprop      <- getProp "typeset" mathjaxobj
              tspropUndef <- valIsUndefined tsprop
              if tspropUndef
                then setTimeout_ window tryInit (Just 50)
                else do
                  liftIO $ raiseLoad mathjax

    _ <- call (makeObject (toJSVal tryInit)) global ()
    pure ()

  dynMathjax <- holdDyn Nothing (Just <$> loadEv)
  let ev = leftmost
        [ attachWithMaybe (\mj m -> fmap (, m) mj)
                          (current dynMathjax)
                          (updated dynMath)
        , attachWithMaybe (\m mj -> fmap (, m) mj)
                          (current dynMath)
                          (updated dynMathjax)
        ]

  _ <- performEvent $ ffor ev $ \(mathjax, m) -> liftJSM $ do
    _ <- mathjax # ("typesetClear" :: Text) $ [[v]]
    setInnerText v m
    _ <- mathjax # ("typeset" :: Text) $ [[v]]
    pure ()

  pure ()
 where
  render' b m = openBrace b <> render m <> closeBrace b
  openBrace :: MathType -> Text
  openBrace Parentheses  = "\\("
  openBrace Square       = "\\["
  openBrace Dollar       = "$"
  openBrace DoubleDollar = "$$"

  closeBrace :: MathType -> Text
  closeBrace Parentheses  = "\\)"
  closeBrace Square       = "\\]"
  closeBrace Dollar       = "$"
  closeBrace DoubleDollar = "$$"

renderEnvDyn
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (Dynamic t (Map MetadataTag Text)) m
     , Prerender js t m
     )
  => Dynamic t (String, [TeXArg], DSum LaTeXTag Identity)
  -> m ()
renderEnvDyn xs = do
  f <- factorDyn $ fmap (\(x, y, z) -> envToDSum x y z) xs
  dyn_ (renderEnv <$> f)

data EnvTag a where
  DMathTag ::EnvTag (DSum LaTeXTag Identity)
  OtherEnvTag ::EnvTag (DSum LaTeXTag Identity)

envToDSum
  :: String -> [TeXArg] -> DSum LaTeXTag Identity -> DSum EnvTag Identity
envToDSum "dmath"  _ x = DMathTag ==> x
envToDSum "dmath*" _ x = DMathTag ==> x
envToDSum _        _ x = OtherEnvTag ==> x

instance GEq EnvTag where
  geq DMathTag    DMathTag    = Just Refl
  geq OtherEnvTag OtherEnvTag = Just Refl
  geq _           _           = Nothing

renderEnv
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (Dynamic t (Map MetadataTag Text)) m
     , Prerender js t m
     )
  => DSum EnvTag (Compose (Dynamic t) Identity)
  -> m ()
renderEnv = \case
  DMathTag :=> Compose x ->
    renderMath (constDyn Square) (dsumToLaTeX . runIdentity <$> x)
  OtherEnvTag :=> Compose x -> renderLaTeX (runIdentity <$> x)

renderCommandDynHead
  :: (Reflex t, DynamicWriter t [Metadata] m)
  => Dynamic t (String, [TeXArg])
  -> m ()
renderCommandDynHead xs = tellDyn (uncurry toMeta <$> xs)
 where
  toMeta "title"  t = pure . Title $ texArgsText t
  toMeta "author" t = pure . Author $ texArgsText t
  toMeta _        _ = []

renderCommandDyn
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (Dynamic t (Map MetadataTag Text)) m
     )
  => Dynamic t (String, [TeXArg])
  -> m ()
renderCommandDyn xs = do
  xs' <- holdUniqDyn xs
  dyn_ (uncurry renderCommand <$> xs')

renderCommand
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (Dynamic t (Map MetadataTag Text)) m
     )
  => String
  -> [TeXArg]
  -> m ()
renderCommand "usepackage" _ = blank
renderCommand "section"    x = el "h2" $ text (texArgsText x)
renderCommand "maketitle"  _ = do
  md <- ask
  _  <- listWithKey md renderMeta
  pure ()
 where
  renderMeta TitleTag  x = el "h1" $ dynText x
  renderMeta AuthorTag x = el "h5" $ dynText x

renderCommand _ _ = blank


texArgsText :: [TeXArg] -> Text
texArgsText = Text.unwords . fmap texArgText

texArgText :: TeXArg -> Text
texArgText = \case
  FixArg  x  -> render x
  OptArg  x  -> render x
  MOptArg xs -> renderCommas xs
  SymArg  x  -> render x
  MSymArg xs -> renderCommas xs
  ParArg  x  -> render x
  MParArg xs -> renderCommas xs

data LaTeXTag a where
  TeXRawTag ::LaTeXTag Text -- ^ Raw text.
  TeXCommTag ::LaTeXTag (String, [TeXArg]) -- ^ Constructor for commands. First argument is the name of the command. Second, its arguments.
  TeXCommSTag ::LaTeXTag String -- ^ Constructor for commands with no arguments. When rendering, no space or {} will be added at the end.
  TeXEnvTag ::LaTeXTag (String, [TeXArg], DSum LaTeXTag Identity) -- ^ Constructor for environments. First argument is the name of the environment. Second, its arguments. Third, its content.
  TeXMathTag ::LaTeXTag (MathType, LaTeX) -- ^ Mathematical expressions.
  TeXLineBreakTag::LaTeXTag (Maybe Measure, Bool) -- ^ Line break command.
  TeXBracesTag ::LaTeXTag (DSum LaTeXTag Identity) -- ^ A expression between braces.
  TeXCommentTag ::LaTeXTag Text -- ^ Comments.
  TeXSeqTag ::LaTeXTag (DSum LaTeXTag Identity, DSum LaTeXTag Identity) -- ^ Sequencing of LaTeX expressions. Use <> preferably.
  TeXEmptyTag ::LaTeXTag ()-- ^ An empty block, Neutral element of <>

laTeXToDSum :: LaTeX -> DSum LaTeXTag Identity
laTeXToDSum = \case
  TeXRaw x         -> TeXRawTag ==> x
  TeXComm x y      -> TeXCommTag ==> (x, y)
  TeXCommS x       -> TeXCommSTag ==> x
  TeXEnv x y z     -> TeXEnvTag ==> (x, y, laTeXToDSum z)
  TeXMath      x y -> TeXMathTag ==> (x, y)
  TeXLineBreak x y -> TeXLineBreakTag ==> (x, y)
  TeXBraces  x     -> TeXBracesTag ==> laTeXToDSum x
  TeXComment x     -> TeXCommentTag ==> x
  TeXSeq x y       -> TeXSeqTag ==> (laTeXToDSum x, laTeXToDSum y)
  TeXEmpty         -> TeXEmptyTag ==> ()

dsumToLaTeX :: DSum LaTeXTag Identity -> LaTeX
dsumToLaTeX = \case
  TeXRawTag       :=> Identity x         -> TeXRaw x
  TeXCommTag      :=> Identity (x, y)    -> TeXComm x y
  TeXCommSTag     :=> Identity x         -> TeXCommS x
  TeXEnvTag       :=> Identity (x, y, z) -> TeXEnv x y (dsumToLaTeX z)
  TeXMathTag      :=> Identity (x, y)    -> TeXMath x y
  TeXLineBreakTag :=> Identity (x, y)    -> TeXLineBreak x y
  TeXBracesTag    :=> Identity x         -> TeXBraces (dsumToLaTeX x)
  TeXCommentTag   :=> Identity x         -> TeXComment x
  TeXSeqTag :=> Identity (x, y) -> TeXSeq (dsumToLaTeX x) (dsumToLaTeX y)
  TeXEmptyTag     :=> Identity ()        -> TeXEmpty

instance GEq LaTeXTag where
  geq TeXRawTag       TeXRawTag       = Just Refl
  geq TeXCommTag      TeXCommTag      = Just Refl
  geq TeXCommSTag     TeXCommSTag     = Just Refl
  geq TeXEnvTag       TeXEnvTag       = Just Refl
  geq TeXMathTag      TeXMathTag      = Just Refl
  geq TeXLineBreakTag TeXLineBreakTag = Just Refl
  geq TeXBracesTag    TeXBracesTag    = Just Refl
  geq TeXCommentTag   TeXCommentTag   = Just Refl
  geq TeXSeqTag       TeXSeqTag       = Just Refl
  geq TeXEmptyTag     TeXEmptyTag     = Just Refl
  geq _               _               = Nothing
