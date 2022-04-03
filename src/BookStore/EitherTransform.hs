module BookStore.EitherTransform where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype EitherT l m r = EitherT { runEitherT :: m (Either l r) }

instance MonadTrans (EitherT l) where
    lift value = EitherT (Right <$> value)

instance (Functor m) => Functor (EitherT l m) where
    fmap f x =
        let mEither = runEitherT x
        in EitherT $ fmap (fmap f) mEither

instance Applicative m => Applicative (EitherT l m) where
    pure x = EitherT $ pure (Right x)
    liftA2 f a b =
        let mEitherA = runEitherT a 
            mEitherB = runEitherT b
            mEitherC = (\eitherA eitherB -> f <$> eitherA <*> eitherB) <$> mEitherA <*> mEitherB
        in EitherT mEitherC

instance (Monad m) => Monad (EitherT l m) where
    return = pure
    x >>= f = EitherT $ do
        eitherX <- runEitherT x
        case eitherX of
            Right x -> runEitherT (f x)
            Left y -> return (Left y)

instance (Monad m, MonadIO m) => MonadIO (EitherT l m) where
    liftIO = lift . liftIO