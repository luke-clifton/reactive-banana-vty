{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reactive.Banana.Dynamic
    ( Dynamic()
    , current
    , updates
    , stepperD
    , valueD
    , valueDLater
    , switchD
    , joinD
    , accumD
    ) where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.These

data Dynamic a = D (Event a) (Behavior a)
    deriving Functor

instance Num a => Num (Dynamic a) where
    fromInteger i = D never (fromInteger i)
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = liftA abs
    signum = liftA signum
    negate = liftA negate

current :: Dynamic a -> Behavior a
current (D _ b) = b

updates :: Dynamic a -> Event a
updates (D e _) = e

stepperD :: MonadMoment m => a -> Event a -> m (Dynamic a)
stepperD initial update = do
    b <- stepper initial update
    pure (D update b)

accumD :: MonadMoment m => a -> Event (a -> a) -> m (Dynamic a)
accumD initial update = do
    e <- accumE initial update
    b <- accumB initial update
    pure (D e b)

switchD :: MonadMoment m => Dynamic a -> Event (Dynamic a) -> m (Dynamic a)
switchD (D e b) update = do
    e' <- switchE e (updates <$> update)
    b' <- switchB b (current <$> update)
    pure (D e' b')

valueD :: MonadMoment m => Dynamic a -> m a
valueD = valueB . current

valueDLater :: MonadMoment m => Dynamic a -> m a
valueDLater = valueBLater . current

joinD :: MonadMoment m => Dynamic (Dynamic a) -> m (Dynamic a)
joinD (D e b) = do
    x <- valueB b
    switchD x e

instance Applicative Dynamic where
    pure a = D never (pure a)

    (<*>) :: forall a b. Dynamic (a -> b) -> Dynamic a -> Dynamic b
    (D e1 b1) <*> (D e2 b2) = D (observeE (f <$> e)) (b1 <*> b2)
        where
            e :: Event (These (a -> b) a)
            e = merge e1 e2

            f :: These (a -> b) a -> Moment b
            f (These f x) = pure (f x)
            f (This f) = f <$> valueB b2
            f (That x) = ($) <$> valueB b1 <*> pure x

instance Semigroup a => Semigroup (Dynamic a) where
    (D e1 b1) <> (D e2 b2) = D (e1 <> e2) (b1 <> b2)

instance Monoid a => Monoid (Dynamic a) where
    mempty = D mempty mempty

nowAndThen :: Dynamic a -> (a -> IO ()) -> MomentIO ()
nowAndThen d f = do
    b <- valueB $ current d
    liftIO (f b)
    reactimate $ f <$> updates d
