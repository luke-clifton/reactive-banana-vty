{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Graphics.Vty.Reactive.Widgets
    where

import Reactive.Banana
import Reactive.Banana.Dynamic
import Graphics.Vty.Reactive.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Graphics.Vty (text', defAttr)
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Semigroup
import qualified Graphics.Vty as Vty
import Control.Monad.Reader
import Data.Foldable
import Data.Traversable

data LabelConfig = LabelConfig
    { labelText  :: Dynamic Text
    , labelWidth :: Dynamic Measure
    , labelAttr  :: Dynamic Vty.Attr
    }

defaultLabelConfig :: LabelConfig
defaultLabelConfig = LabelConfig
    { labelText = pure mempty
    , labelWidth = pure (Measure 10 0 0)
    , labelAttr = pure $ defAttr
    }
 
label :: LabelConfig -> UI ()
label LabelConfig{..} = mdo
    drawWidget WidgetConfig
        { widgetWidth = labelWidth
        , widgetHeight = pure 1
        , widgetImage = renderFill <$> labelAttr <*> labelText
        }
    pure ()

    where
        renderFill :: Vty.Attr -> Text -> FlexImage
        renderFill attr txt w h = Vty.resize w h (text' attr (fixText w txt))

fixText :: Int -> Text -> Text
fixText target t =
    let
        iw = Vty.wctwidth t
    in
        case compare target iw of
            LT -> Text.take (target - 1) t <> "…"
            EQ -> t
            GT -> t <> Text.replicate (target - iw) " "


data ScrollControl = ScrollUp | ScrollDown

scrollToOffsetChange :: Int -> ScrollControl -> Int -> Int
scrollToOffsetChange max ScrollUp 0 = 0
scrollToOffsetChange max ScrollUp x = pred x
scrollToOffsetChange max ScrollDown x
    | max <= x  = max
    | otherwise = succ x

basicScroll :: Vty.Event -> Maybe ScrollControl
basicScroll (Vty.EvKey Vty.KUp _) = Just ScrollUp
basicScroll (Vty.EvKey Vty.KDown _) = Just ScrollDown
basicScroll _ = Nothing

data ListConfig a = ListConfig
    { listItems    :: Dynamic [a]
    , listControls :: Event ScrollControl
    , listRender   :: Dynamic (a -> UI ())
    , listWidth    :: Dynamic Measure
    , listHeight   :: Dynamic Measure
    }

defaultListConfig :: ListConfig a
defaultListConfig = ListConfig
    { listItems = pure []
    , listControls = never
    , listRender = pure (const $ pure ())
    , listWidth = pure (Measure 0 1 0)
    , listHeight = pure (Measure 0 1 0)
    }

list :: forall a. ListConfig a -> UI ()
list ListConfig{..} = do
    let listLength = length <$> listItems
    listScroll <- accumD 0 (current (scrollToOffsetChange <$> listLength - 1) <@> listControls)
    let
        dui = fmap sequenceA_ $ fmap <$> listRender <*> (drop <$> listScroll <*> listItems)
    dynUI dui
    pure ()

data TextListConfig a = TextListConfig
    { textListItems    :: Dynamic [a]
    , textListControls :: Event ScrollControl
    , textListRender   :: Dynamic (a -> Text)
    , textListWidth    :: Dynamic Measure
    , textListHeight   :: Dynamic Measure
    }

defaultTextListConfig :: TextListConfig a
defaultTextListConfig = TextListConfig
    { textListItems = pure []
    , textListControls = never
    , textListRender = pure (const "")
    , textListWidth = pure (Measure 0 1 0)
    , textListHeight = pure (Measure 0 1 0)
    }

textList :: forall a. TextListConfig a -> UI ()
textList TextListConfig{..} = do
    let textListLength = length <$> textListItems
    textListScroll <- accumD 0 (current (scrollToOffsetChange <$> textListLength - 1) <@> textListControls)
    drawWidget WidgetConfig
        { widgetWidth = textListWidth
        , widgetHeight = textListHeight
        , widgetImage = render <$> textListScroll <*> textListItems <*> textListRender
        }
    
    where
        render :: Int -> [a] -> (a -> Text) -> FlexImage
        render scroll items f w h =
            let
                itemsToRender = take h $ drop scroll items
                textsToRender = map (fixText w . f) itemsToRender
                images = map (Vty.text' Vty.defAttr) textsToRender
            in
                Vty.vertCat images
                
data FillConfig = FillConfig
    { fillWidth :: Dynamic Measure
    , fillHeight :: Dynamic Measure
    , fillChar :: Dynamic Char
    }

defaultFillConfig :: FillConfig
defaultFillConfig = FillConfig
    { fillWidth = pure (Measure 0 1 0)
    , fillHeight = pure (Measure 0 1 0)
    , fillChar = pure ' '
    }

fill :: FillConfig -> UI ()
fill FillConfig {..} = do
    drawWidget $ WidgetConfig
        { widgetWidth  = fillWidth
        , widgetHeight = fillHeight
        , widgetImage  = (\f w h -> Vty.charFill Vty.defAttr f w h) <$> fillChar
        }

center :: Measure -> UI a -> UI a
center p ui = horizontally $ do
    horizontally $ do
        fill FillConfig
            { fillWidth = pure p
            , fillHeight = 1
            , fillChar = pure ' '
            }
        a <- vertically $ do
            fill FillConfig
                { fillHeight = pure p
                , fillWidth = 1
                , fillChar = pure ' '
                }
            a <- ui
            fill FillConfig
                { fillHeight = pure p
                , fillWidth = 1
                , fillChar = pure ' '
                }
            pure a
        fill FillConfig
            { fillWidth = pure p
            , fillHeight = 1
            , fillChar = pure ' '
            }
        pure a


single :: Char -> UI ()
single c = fill FillConfig {fillWidth = 1, fillHeight = 1, fillChar = pure c}

strut :: UI ()
strut = fill FillConfig {fillWidth = 1, fillHeight = pure (Measure 0 1 0), fillChar = pure '|'}

data SpinnerConfig = SpinnerConfig
    { spinnerUpdate  :: Event ()
    , spinnerSymbols :: String
    }

defaultSpinnerConfig :: SpinnerConfig
defaultSpinnerConfig = SpinnerConfig
    { spinnerUpdate = never
    , spinnerSymbols = "⣾⣷⣯⣟⡿⢿⣻⣽"
    }

spinner :: SpinnerConfig -> UI ()
spinner SpinnerConfig{..} = do
    ix <- accumD 0 (succ <$ spinnerUpdate)
    let
        symbolCount = length spinnerSymbols
        currentIx = (`mod` symbolCount) <$> ix
        currentChar = (spinnerSymbols !!) <$> currentIx
    
    drawWidget WidgetConfig
        { widgetWidth = 1
        , widgetHeight = 1
        , widgetImage = render <$> currentChar
        }

    where
        render :: Char -> FlexImage
        render c _ _ = Vty.string Vty.defAttr [c]

box :: UI () -> UI ()
box ui = do
    horizontally $ do
        vertically $ do
            fill defaultFillConfig
                { fillWidth = 1
                , fillHeight = 1
                , fillChar = pure '┌'
                }
            fill defaultFillConfig
                { fillWidth = 1
                , fillChar = pure '│'
                }
            fill defaultFillConfig
                { fillWidth = 1
                , fillHeight = 1
                , fillChar = pure '└'
                }
        vertically $ do
            fill defaultFillConfig
                { fillHeight = 1
                , fillChar = pure '─'
                }
            ui
            fill defaultFillConfig
                { fillHeight = 1
                , fillChar = pure '─'
                }
        vertically $ do
            fill defaultFillConfig
                { fillWidth = 1
                , fillHeight = 1
                , fillChar = pure '┐'
                }
            fill defaultFillConfig
                { fillWidth = 1
                , fillChar = pure '│'
                }
            fill defaultFillConfig
                { fillWidth = 1
                , fillHeight = 1
                , fillChar = pure '┘'
                }

data InputControl
    = InputChar Char
    | InputBS
    | InputNewLine
    | InputSubmit
    | InputForward
    | InputBackward
    deriving Eq

data InputConfig = InputConfig
    { inputInput :: Event InputControl
    , inputWidth :: Dynamic Measure
    , inputHeight :: Dynamic Measure
    }

data Input = Input
    { inputValue :: Dynamic Text
    , inputSubmit :: Event Text
    }

defaultInputConfig :: InputConfig
defaultInputConfig = InputConfig
    { inputWidth = pure (Measure 0 1 0)
    , inputHeight = pure (Measure 0 1 0)
    , inputInput = never
    }

basicInput :: Vty.Event -> Maybe InputControl
basicInput (Vty.EvKey (Vty.KChar c) []) = Just (InputChar c)
basicInput (Vty.EvKey (Vty.KBS) [])     = Just InputBS
basicInput (Vty.EvKey (Vty.KEnter) [])   = Just InputNewLine
basicInput (Vty.EvKey (Vty.KEnter) _)    = Just InputSubmit
basicInput (Vty.EvKey (Vty.KLeft) _)     = Just InputBackward
basicInput (Vty.EvKey (Vty.KRight) _)     = Just InputForward
basicInput _ = Nothing

inputAccum :: InputControl -> (Text, Text) -> (Text, Text)
inputAccum (InputChar c) (a, b) = (Text.snoc a c, b)
inputAccum InputBS ("", b) = ("", b)
inputAccum InputBS (a, b) = (Text.init a, b)
inputAccum InputNewLine (a, b) = (Text.snoc a '\n', b)
inputAccum InputSubmit a = a
inputAccum InputForward (a,b) = case Text.uncons b of
    Nothing -> (a,b)
    Just (c,b) -> (Text.snoc a c, b)
inputAccum InputBackward (a,b) = case Text.unsnoc a of
    Nothing -> (a,b)
    Just (a,c) -> (a, Text.cons c b)

input :: InputConfig -> UI Input
input InputConfig{..} = do
    value <- accumD ("","") (inputAccum <$> inputInput)
    let
        theText = uncurry (<>) <$> value
        lines = Text.lines <$> theText
    drawWidget WidgetConfig
        { widgetWidth = inputWidth
        , widgetHeight = inputHeight
        , widgetImage = render <$> lines
        }
    pure Input
        { inputValue = theText
        , inputSubmit = current theText <@ filterE (== InputSubmit) inputInput
        }
    where
        render :: [Text] -> FlexImage
        render l w h = Vty.vertCat $ map (Vty.text' Vty.defAttr) l
