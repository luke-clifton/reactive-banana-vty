{-# LANGUAGE OverloadedStrings #-}
module Graphics.Vty.Reactive
    where

import Control.Monad
import Control.Exception
import Graphics.Vty.Reactive.Types
import Graphics.Vty.Reactive.Widgets
import qualified Graphics.Vty as Vty
import qualified Data.Text as Text
import Reactive.Banana

runStandardUI :: (Event () -> UI ()) -> IO ()
runStandardUI u = do
    cfg <- Vty.standardIOConfig
    vty <- Vty.mkVty cfg
    ui vty u `finally` Vty.shutdown vty

testSpinner :: IO ()
testSpinner = runStandardUI $ \e -> vertically $ do
    replicateM_ 20 $ horizontally $ replicateM_ 20 $ spinner defaultSpinnerConfig {spinnerUpdate = e}

test6 :: IO ()
test6 = runStandardUI $ \e -> do
    center (Measure 0 1 0) $
        label defaultLabelConfig
            { labelText = pure "Hello"
            , labelAttr = pure $ Vty.defAttr `Vty.withStyle` Vty.reverseVideo
            }

test5 :: IO ()
test5 = runStandardUI $ \e -> do
    t <- accumE (pure ()) ((center (Measure 1 1 0) (fill FillConfig {fillWidth = 2, fillHeight = 2, fillChar = pure 'a'}) >>) <$ e)
    vertically $ switchUI (pure ()) t
    pure ()

test4 :: IO ()
test4 = runStandardUI $ \e -> do
    t <- accumE (pure ()) ((box (center (Measure 1 1 0) (box $ fill FillConfig {fillWidth = 2, fillHeight = 2, fillChar = pure 'a'})) >>) <$ e)
    switchUI (pure ()) t
    pure ()

test1 :: IO ()
test1 = runStandardUI $ \_ -> void $  do
    vertically $ replicateM 4 $ do
        horizontally $ replicateM 4 $ do
            center (Measure 0 1 0) $ do
                    fill FillConfig
                        { fillWidth = pure (Measure 1 2 0)
                        , fillHeight = pure (Measure 1 2 0)
                        , fillChar = pure 'a'
                        }
        

test :: IO ()
test = runStandardUI $ \_ -> do
    vertically $ do
        horizontally $ do
            fill FillConfig
                { fillWidth = 1
                , fillHeight = 1
                , fillChar = pure 'a'
                }
            fill FillConfig
                { fillWidth  = pure $ Measure 0 1 0
                , fillHeight = pure $ Measure 0 1 0
                , fillChar = pure '-'
                }
            fill FillConfig
                { fillWidth = 1
                , fillHeight = 1
                , fillChar = pure 'b'
                }
        horizontally $ do
            center (Measure 0 1 0) $ do
                fill FillConfig
                    { fillWidth  = pure $ Measure 3 0 0
                    , fillHeight = pure $ Measure 3 0 0
                    , fillChar = pure 'a'
                    }
            fill FillConfig
                { fillWidth  = pure $ Measure 1 0 0
                , fillHeight = pure $ Measure 0 1 0
                , fillChar = pure '|'
                }
            center (Measure 0 1 0) $ do
                fill FillConfig
                    { fillWidth  = pure $ Measure 3 0 0
                    , fillHeight = pure $ Measure 3 0 0
                    , fillChar = pure 'a'
                    }
