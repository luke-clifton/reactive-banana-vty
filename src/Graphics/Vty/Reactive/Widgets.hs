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

fill' :: (FillConfig -> FillConfig) -> UI ()
fill' f = fill (f $ FillConfig (pure (Measure 0 1 0)) (pure (Measure 0 1 0)) (pure ' '))

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
-- 
-- snugBox :: UI () -> UI ()
-- snugBox = box Snug Snug
-- 
-- flexBox :: UI () -> UI ()
-- flexBox = box Flex Flex
-- 
-- 
-- centerHoriz :: UI a -> UI a
-- centerHoriz (UI ui) = horizontally $ do
--     uiread <- UI ask
--     let
--         w = runReaderT ui uiread
--         m = runWriterT w
--     (a, r) <- UI $ lift $ lift $ m
--     
--     let
--         flexHeights = sum . fmap flexComponent <$> uiwHeights r
--         strutHeight = (\x -> if x == 0 then Measure 0 1 else 0) <$> flexHeights
-- 
--         flexWidths = sum . fmap flexComponent <$> uiwWidths r
--         strutWidth = (\x -> if x == 0 then Measure 0 1 else 0) <$> flexWidths
-- 
--     spacer' strutWidth
--     UI $ tell r
--     spacer' strutWidth
--     pure a
-- 
-- centerVert :: UI a -> UI a
-- centerVert (UI ui) = vertically $ do
--     uiread <- UI ask
--     let
--         w = runReaderT ui uiread
--         m = runWriterT w
--     (a, r) <- UI $ lift $ lift $ m
--     
--     let
--         flexHeights = sum . fmap flexComponent <$> uiwHeights r
--         strutHeight = (\x -> if x == 0 then Measure 0 1 else 0) <$> flexHeights
-- 
--         flexWidths = sum . fmap flexComponent <$> uiwWidths r
--         strutWidth = (\x -> if x == 0 then Measure 0 1 else 0) <$> flexWidths
-- 
--     strut' strutHeight
--     UI $ tell r
--     strut' strutHeight
--     pure a
