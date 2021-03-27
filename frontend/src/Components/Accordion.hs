{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Components.Accordion
  ( accordion
  , accordionStyle
  , StepperState(..)
  , stackview
  , stackviewEmpty
  , stackviewRow
  , stepper
  , stepperPage
  , StepperPage
  )
where

import           Prelude                 hiding ( rem )

import           Clay                    hiding ( icon )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.State
import           Data.Default
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Reflex.Dom              hiding ( display
                                                , (&)
                                                )

import           Components.Class
import           Components.Input
import           Components.Icon
import           Nordtheme

data StepperState = StepperNeutral
                    | StepperSuccess
                    | StepperError
                    deriving (Eq, Show)

instance Default StepperState where
  def = StepperNeutral

accordionStyle :: Css
accordionStyle = mconcat [accordionStyle', stackviewStyle, stepperStyle]

accordionStyle' :: Css
accordionStyle' = ".accordion" ? do
  input # ("type" @= "checkbox") ? display none
  border solid (px 1) grey0'
  borderBottomWidth nil

  ".angle-icon" ? do
    marginRight (rem 0.5)
    transforms [translateY (rem 0.25), rotate (deg 90)]

  label ? do
    display flex
    padding (rem (1 / 4)) (rem (1 / 2)) (rem (1 / 4)) (rem (1 / 2))

    cursor pointer
    hover & backgroundColor nord6'

  ".content" ? do
    display none
    backgroundColor white
    borderTop solid (px 1) grey0'
    padding (rem (3 / 2)) (rem 2) (rem (3 / 2)) (rem 2)

  input # checked |+ star ? do
    ".angle-icon" ? transforms [translateY (rem 0.25), rotate (deg 180)]
    ".content" ? display block
    label ? backgroundColor nord4'

  firstOfType & do
    borderTopLeftRadius (px 3) (px 3)
    borderTopRightRadius (px 3) (px 3)

    label & do
      borderTopLeftRadius (px 3) (px 3)
      borderTopRightRadius (px 3) (px 3)


  lastOfType & do
    borderBottomWidth (px 1)
    borderBottomLeftRadius (px 3) (px 3)
    borderBottomRightRadius (px 3) (px 3)

    ".content" ? do
      borderBottomLeftRadius (px 3) (px 3)
      borderBottomRightRadius (px 3) (px 3)

  ".disabled" & do
    ".angle-icon" ? visibility hidden
    label ? do
      cursor cursorDefault
      hover & backgroundColor inherit

stackviewStyle :: Css
stackviewStyle = do
  ".stack-row" ? do
    flexGrow 1
    display inlineGrid
    "grid-template-columns" -: "1fr 2fr"
    input ? do
      paddingBottom nil
      marginTop (rem (-1 / 4))
    ".helptext" ? marginAll nil

  ".content" |> ".stack-row" # firstOfType ? marginTop (rem (-3 / 2))
  ".content" |> ".stack-row" ? do
    padding (rem (1 / 2)) (rem (1 / 2)) (rem (1 / 2)) (rem 2)
    marginLeft (rem (-2))
    marginRight (rem (-2))
    borderBottom solid (px 1) grey0'
  ".content" |> ".stack-row" # lastOfType ? do
    marginBottom (rem (-3 / 2))
    borderBottomWidth nil

  ".stack-view" Clay.** input # checked |+ star Clay.** ".content" ? do
    display flex
    flexDirection column

stepperStyle :: Css
stepperStyle = ".stepper" ? do
  label Clay.? do
    borderLeft solid (rem (1 / 4)) (lighten 0.5 grey0')
    paddingLeft (rem (1 / 4))
    paddingTop (rem (3 / 4))
    paddingBottom (rem (3 / 4))

  ".error" Clay.& label ? borderLeftColor nord11'

  ".success" Clay.& label ? borderLeftColor green1'

  ".stepnum" ? do
    display inlineFlex
    flexDirection column
    justifyContent center
    width (rem (3 / 2))
    minWidth (rem (3 / 2))

stepper'
  :: (MonadIO m, DomBuilder t m, PostBuild t m)
  => Text
  -> Dynamic t Bool
  -> Dynamic t StepperState
  -> Bool
  -> Event t Bool
  -> m b
  -> m a
  -> m (b, a)
stepper' cls disabledDyn state' initOpen setOpen titl cnt =
  elDynClass "section" (stateCls <$> state' <*> disabledDyn) $ do
    idStr <- randomId
    _     <- dyn (mkCheckbox idStr <$> disabledDyn)
    el "div" $ do
      l <- elAttr "label" ("class" =: "p3" <> "for" =: idStr) $ do
        icon def { _iconConfig_class = constDyn $ Just "angle-icon" } angleIcon
        titl

      c <- elClass "div" "content" cnt
      pure (l, c)

 where
  mkCheckbox _     True  = pure ()
  mkCheckbox idStr False = checkboxInputSimple initOpen setOpen $ "id" =: idStr

  stateCls _              True = "accordion disabled" <> cls
  stateCls StepperNeutral _    = "accordion" <> cls
  stateCls StepperError   _    = "accordion error" <> cls
  stateCls StepperSuccess _    = "accordion success" <> cls

accordion'
  :: (MonadIO m, DomBuilder t m, PostBuild t m)
  => Text
  -> Event t Bool
  -> m b
  -> m a
  -> m (b, a)
accordion' cls = stepper' cls (constDyn False) (constDyn def) False

accordion
  :: (MonadIO m, DomBuilder t m, PostBuild t m)
  => Event t Bool
  -> Dynamic t Text
  -> m a
  -> m a
accordion setOpen titl = fmap snd . accordion' mempty setOpen (dynText titl)

accordionEmpty :: (DomBuilder t m, MonadIO m, PostBuild t m) => m b -> m b
accordionEmpty titl = fst <$> stepper' mempty
                                       (constDyn True)
                                       (constDyn def)
                                       False
                                       never
                                       titl
                                       (pure ())

stackviewEmpty
  :: (DomBuilder t m, MonadIO m, PostBuild t m) => Dynamic t Text -> m a -> m a
stackviewEmpty titl = accordionEmpty . stackviewRow titl

stackview
  :: (MonadIO m, DomBuilder t m, PostBuild t m)
  => Event t Bool
  -> m b
  -> m a
  -> m (b, a)
stackview = accordion' " stack-view"

stackviewRow :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m a
stackviewRow titl cnt = elClass "div" "stack-row" $ do
  dynText titl
  elClass "div" "stack-content" cnt


type StepperPage t m a = StateT (Integer, Event t ()) m a

stepper
  :: (MonadIO m, DomBuilder t m, PostBuild t m)
  => StepperPage t m (Dynamic t a)
  -> m (Event t a)
stepper cnt = do
  postBuildEv           <- getPostBuild
  (dynA, (_, submitEv)) <- el "div" $ runStateT cnt (0, postBuildEv)
  pure $ tagPromptlyDyn dynA submitEv

stepperPage
  :: (MonadIO m, DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Text
  -> Text
  -> m (a, Event t StepperState)
  -> StepperPage t m a
stepperPage titl descr cnt = StateT $ \(stepNum, openEv) -> do
  let setOpenEv = True <$ openEv
  canOpenDyn <- foldDyn (||) False setOpenEv

  rec ((), (b, stateEv)) <- stepper'
        " stepper"
        (Prelude.not <$> canOpenDyn)
        stateDyn
        True
        (leftmost [setOpenEv, False <$ successEv])
        (heading stepNum stateDyn)
        cnt
      stateDyn <- holdDyn def stateEv
      let successEv = ffilter (== StepperSuccess) stateEv

  pure (b, (stepNum + 1, () <$ successEv))
 where
  heading stepNum stateDyn = do
    _ <- elClass "span" "stepnum" $ dyn (numOrIcon stepNum <$> stateDyn)
    stackviewRow (constDyn titl) (text descr)

  numOrIcon _ StepperSuccess = icon
    def { _iconConfig_status = constDyn (Just Success) }
    successStandardIcon
  numOrIcon _ StepperError =
    icon def { _iconConfig_status = constDyn (Just Danger) } errorStandardIcon
  numOrIcon stepNum _ = text ((<> ".") . pack . show $ stepNum)

