{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vty.Reactive.Types
    where


import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Dynamic

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans
import Data.Functor.Identity
import Data.Monoid
import Data.Semigroup
import Data.Unique
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Traversable

import Graphics.Vty (Image, horizJoin, vertJoin, Vty)
import qualified Graphics.Vty as Vty

newtype UI a = UI
    { unUI :: WriterT (Dynamic [WS]) MomentIO a
    } deriving newtype (Functor, Applicative, Monad, MonadFix)

instance MonadMoment UI where
    liftMoment = UI . lift . liftMoment

data Measure = Measure
    { measureFix  :: Int -- ^ The fixed component of the size. Will always be at least this big
    , measureFlex :: Int -- ^ The flex component of the size. Leftover space will be divided up based on ratio of flex.
    , measureFill :: Int -- ^ A low priority flex. If no flex is in any elements, fill will be used. Otherwise it's ignored.
    } deriving (Eq, Ord, Show)

instance Num Measure where
    Measure a b c + Measure x y z = Measure (a + x) (b + y) (c + z)
    fromInteger i = Measure (fromInteger i) 0 0

data WS = WS
    { wsWidth  :: Measure
    , wsHeight :: Measure
    , wsImage  :: Int -> Int -> Image
    }

data RS = RS
    { rsContainerWidth :: Dynamic Int
    , rsContainerHeight :: Dynamic Int
    }

expand :: Int -> Measure -> Int
expand max (Measure a b c)
    | b == 0 && c == 0 = a
    | otherwise        = max

drawWS :: Dynamic WS -> UI ()
drawWS = UI . tell . fmap (:[])

dynIx :: Dynamic [a] -> Int -> Dynamic a
dynIx d x = fmap (!! x) d

-- divideIn :: ((Dynamic Int -> Dynamic Int -> Dynamic Image) -> Dynamic Int -> Dynamic Int -> Dynamic Image) -> Dynamic Int -> Dynamic Int -> (WS -> Dynamic Measure) -> (WS -> Dynamic Measure) -> [WS] -> [Dynamic Image]
-- divideIn f varSize fixSize variable fixed ws =
--     let
--         fixedVar = sum $ fmap measureFix . variable <$> ws
--         remainingVar = varSize - fixedVar
--         flexVar = sum $ fmap measureFlex . variable <$> ws
--         vars = divideSize <$> flexVar <*> remainingVar <*> sequenceA (variable <$> ws) :: Dynamic [Int]
--         fixs = fixed <$> ws
--         images = zipWith (\x r -> f r (dynIx vars x) (expand <$> fixSize <*> (fixs !! x))) [0..] $ wsImage <$> ws :: [Dynamic Image]
--     in images

switchUI :: UI a -> Event (UI a) -> UI (Dynamic a)
switchUI ui e = do
    (a, image) <- UI $ lift $ runWriterT $ unUI $ ui
    x <- UI $ lift $ execute (runWriterT . unUI <$> e)
    switchD image (snd <$> x) >>= (UI . tell)
    as <- stepperD a (fst <$> x)
    pure as
    
type FlexImage = Int -> Int -> Image

vertically :: UI a -> UI a
vertically ui = do
    let
    (a, wss) <- UI $ lift $ runWriterT $ unUI $ ui
    let
        hm = sum . fmap wsHeight <$> wss :: Dynamic Measure
        -- TODO: maximum here (and in `horizontally` is maybe not
        -- quite what we want? We might like to inherit some flex?
        wm = maximum . (0:) . fmap wsWidth <$> wss :: Dynamic Measure
        
    drawWidget $ WidgetConfig
        { widgetWidth = wm
        , widgetHeight = hm
        , widgetImage = render <$> wss
        }
    pure a
        where
            render :: [WS] -> FlexImage
            render ws w h =
                let
                    fixedHeight     = sum $ measureFix . wsHeight <$> ws :: Int
                    remainingHeight = h - fixedHeight :: Int
                    flexHeight      = sum $ measureFlex . wsHeight <$> ws :: Int
                    heights         = divideSize flexHeight remainingHeight (wsHeight <$> ws)
                    images          = zipWith (\h f -> wsImage f (expand w (wsWidth f)) h) heights ws :: [Image]
                in Vty.vertCat images

horizontally :: UI a -> UI a
horizontally ui = do
    let
    (a, wss) <- UI $ lift $ runWriterT $ unUI $ ui
    let
        wm = sum . fmap wsWidth <$> wss :: Dynamic Measure
        hm = maximum . (0:) . fmap wsHeight <$> wss :: Dynamic Measure
        
    drawWidget $ WidgetConfig
        { widgetWidth = wm
        , widgetHeight = hm
        , widgetImage = render <$> wss
        }
    pure a
        where
            render :: [WS] -> FlexImage
            render ws w h =
                let
                    fixedWidth     = sum $ measureFix . wsWidth <$> ws :: Int
                    remainingWidth = w - fixedWidth :: Int
                    flexWidth      = sum $ measureFlex . wsWidth <$> ws :: Int
                    widths         = divideSize flexWidth remainingWidth (wsWidth <$> ws)
                    images         = zipWith (\w f -> wsImage f w (expand h (wsHeight f))) widths ws :: [Image]
                in Vty.horizCat images

drawWidget :: WidgetConfig -> UI ()
drawWidget WidgetConfig{..} = UI $ tell $ fmap (:[]) $ liftA3 WS widgetWidth widgetHeight widgetImage

data WidgetConfig = WidgetConfig
    { widgetWidth :: Dynamic Measure
    , widgetHeight :: Dynamic Measure
    , widgetImage :: Dynamic FlexImage
    }


runUI :: Dynamic Int -> Dynamic Int -> UI a -> MomentIO (a, Dynamic Image)
runUI w h ui = do
    (a, ws) <- runWriterT $ unUI $ horizontally ui
    let x = wsImage . head <$> ws
    pure (a, x <*> w <*> h)


divideSize :: forall t. Traversable t => Int -> Int -> t Measure -> t Int
divideSize ft fa m = snd $ mapAccumL go (ft, fa) m
    where
        go :: (Int, Int) -> Measure -> ((Int, Int), Int)
        go (0, flexAvailable) (Measure fixed flex _) = ((0, flexAvailable), fixed)
        go (flexTotal, flexAvailable) (Measure fixed flex _) =
            let
                flexConsumed = flexAvailable * flex `div` flexTotal
            in ((flexTotal - flex, flexAvailable - flexConsumed), fixed + flexConsumed)

ui :: Vty -> (Event () -> UI a) -> IO ()
ui vty app = do
    (startHandler, fireStart) <- newAddHandler
    (tickHandler, fireTick) <- newAddHandler
    (resizeHandler, fireResize) <- newAddHandler
    region <- Vty.displayBounds $ Vty.outputIface vty
    let
        render :: Image -> IO ()
        render i = Vty.update vty Vty.Picture
            { picCursor = Vty.NoCursor
            , picLayers = [i]
            , picBackground = Vty.Background ' ' Vty.defAttr
            }

        networkDescription :: MomentIO ()
        networkDescription = do
            size <- stepperD region =<< fromAddHandler resizeHandler
            tick <- fromAddHandler tickHandler
            (_, image) <- runUI (fst <$> size) (snd <$> size) (app tick)
            imageUpdates <- changes $ current $ image
            start <- fromAddHandler tickHandler
            initialImage <- valueB $ current image
            liftIO $ render initialImage
            reactimate' (fmap render <$> imageUpdates)
            pure ()
    net <- compile networkDescription
    actuate net
    fireStart ()
    let
        loop :: IO ()
        loop = do
            Vty.nextEvent vty >>= \case
                Vty.EvKey Vty.KEsc _ -> pure ()
                Vty.EvResize x y -> fireResize (x, y) >> loop
                _ -> fireTick () >> loop

    loop

