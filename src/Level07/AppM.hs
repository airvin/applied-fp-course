{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Level07.AppM
  ( AppM (..)
  , App
  , Env (..)
  , liftEither
  , runApp
  , ask
  ) where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..))
import           Control.Applicative    (liftA2)

import           Data.Bifunctor         (first, second)
import           Data.Text              (Text)

import           Level07.Types          (Conf, FirstAppDB)
import           Level07.Types.Error    (Error)

-- | First, let's clean up our (Conf,FirstAppDB) with an application Env type.
-- We will add a general purpose logging function as well. Remember that
-- functions are values, we're able to pass them around and place them on
-- records like any other type.
data Env = Env
  -- | We will add a function to take some 'Text' input and print it to the
  -- console as a crude form of logging. Construct a function that matches this
  -- type so you can include it when you create the 'Env'.
  { envLoggingFn :: Text -> App ()

  -- We're able to nest records to keep things neat and tidy.
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

-- | It would be nice to remove the need to pass around our Env to every
-- function that needs it. Wouldn't it be great to have our functions run where
-- we could simply ask for the current Env?
--
-- We can create this by wrapping a function in a newtype like so:
--
-- This gives us a type that declares this function has access to our Env, and
-- will do something involving IO. It's another form of documentation and type
-- safety. AppM only has one definition and so we can easily understand what it
-- implies when used in our application.
newtype AppM e a = AppM
  { runAppM :: Env -> IO (Either e a)
  }
  -- | Quite often, GHC is able to write the code for us. In this case we just
  -- tell GHC that we want a Functor instance for our newtype, and it is able to
  -- correctly derive what is needed.
  deriving Functor

  -- | We could do this for the rest of these instances, but that would turn
  -- into "magic" what is otherwise straight-forward implementations. You are
  -- here to learn after all.

type App = AppM Error

runApp :: App a -> Env -> IO (Either Error a)
runApp = runAppM 

instance Applicative (AppM e) where
  pure :: a -> AppM e a
  pure a = AppM (\_ -> pure $ Right a)  

  (<*>) :: AppM e (a -> b) -> AppM e a -> AppM e b
  -- Solution 1: using bind
  -- (<*>) appMfn appMa = do
  --   a <- appMa
  --   fn <- appMfn
  --   pure $ fn a
  -- Solution 2: using two binds on the IOs
  -- (<*>) appMfn appMa = AppM (\env -> do
  --     let ioEitherFn = runAppM appMfn env
  --     let ioEithera = runAppM appMa env
  --     eitherFn <- ioEitherFn
  --     eithera <- ioEithera
  --     pure $ eitherFn <*> eithera
  --   )
  -- Solution 3: using one bind on the IO
  -- (<*>) appMfn appMa = AppM (\env -> let
  --   ioEitherFn = runAppM appMfn env
  --   ioEithera = runAppM appMa env
  --   in 
  --     ioEitherFn >>= (\eitherFn -> (eitherFn <*>) <$> ioEithera))
  -- Solution 4: No binds
  -- (<*>) appMfn appMa = AppM (\env -> let
  --   ioEitherFn = runAppM appMfn env 
  --   ioEithera = runAppM appMa env
  --   in 
  --     fmap (<*>) ioEitherFn <*> ioEithera)
  -- Solution 5: using lift
  (<*>) appMfn appMa = AppM (\env -> let
    ioEitherFn = runAppM appMfn env 
    ioEithera = runAppM appMa env
    in 
      liftA2 (<*>) ioEitherFn ioEithera)

-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap (<*>) :: k (F (a -> b)) -> k (F a -> F b)
-- <$> :: (a -> b) -> F a -> F b
-- <*> :: F (a -> b) -> F a -> F b

instance Monad (AppM e) where
  -- | When it comes to running functions in (AppM e) as a Monad, this will take
  -- care of passing the Env from one function to the next whilst preserving the
  -- error handling behaviour.
  (>>=) :: AppM e a -> (a -> AppM e b) -> AppM e b
  -- Solution 1: ugly pattern matching
  -- (>>=) appMa a2appMb = AppM (\env -> let
  --   ioEithera = runAppM appMa env
  --   in
  --     ioEithera >>= (\eitherA -> case (a2appMb <$> eitherA) of
  --       Right appMb -> runAppM appMb env
  --       Left e -> pure $ Left e))
  -- Solution 2: using either
  (>>=) appMa a2appMb = AppM (\env -> let
    ioEithera = runAppM appMa env
    in
      -- ioEithera >>= (\eitherA -> either (pure . Left) (\a -> runAppM (a2appMb a) env) eitherA))
      -- ioEithera >>= either (pure . Left) (\a -> runAppM (a2appMb a) env))
      -- ioEithera >>= either (pure . Left) (\a -> flip runAppM env (a2appMb a)))
      ioEithera >>= either (pure . Left) (flip runAppM env . a2appMb))


instance MonadError e (AppM e) where
  throwError :: e -> AppM e a
  throwError e = AppM (\_ -> pure $ Left e)

  catchError :: AppM e a -> (e -> AppM e a) -> AppM e a
  catchError appMa e2AppMa = AppM (\env -> let
    ioEitherA = runAppM appMa env
    in 
      -- ioEitherA >>= (\eitherA -> case eitherA of 
      --   Left e -> runAppM (e2AppMa e) env
      --   Right a -> pure $ Right a))
      ioEitherA >>= (\eitherA -> either 
        (flip runAppM env . e2AppMa) 
        (pure . Right)
        eitherA))  

-- either :: (e -> c) -> (a -> c) -> either e a -> c
-- where c is IO Either e b

instance MonadReader Env (AppM e) where
  -- Return the current Env from the AppM.
  ask :: AppM e Env
  ask = reader id

  -- Run a (AppM e) inside of the current one using a modified Env value.
  local :: (Env -> Env) -> AppM e a -> AppM e a
  local fn appMa = AppM (\env -> runAppM appMa $ fn env)

  -- This will run a function on the current Env and return the result.
  reader :: (Env -> a) -> AppM e a
  reader fn = AppM (\env -> pure $ Right $ fn env)

instance MonadIO (AppM e) where
  -- Take a type of 'IO a' and lift it into our (AppM e).
  liftIO :: IO a -> AppM e a
  -- liftIO ioA = AppM (\_ -> ioA >>= (\a -> pure $ Right a)) 
  liftIO ioA = AppM (\_ -> Right <$> ioA)

-- | This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither :: Either e a -> AppM e a
liftEither = either throwError (\a -> AppM (\_ -> pure $ Right a)) 

-- either :: (e -> c) -> (a -> c) -> either e a -> c
-- where c is AppM e a

-- Move on to ``src/Level07/DB.hs`` after this
