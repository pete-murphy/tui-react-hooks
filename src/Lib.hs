{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib where

import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Data.Maybe
import qualified Data.TMap as TM
import qualified Data.Text.Lazy as TL
import Data.Typeable
import qualified Graphics.Vty as Vty
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Control.Monad.STM

newtype React a = React
  { runReact :: ReaderT TM.TMap IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader TM.TMap
    )

newtype Component props = Component
  { runComponent :: props -> React Vty.Picture
  }

newtype StateMap = StateMap (TVar TM.TMap)

-- | Context
withContext :: Typeable ctx => ctx -> React a -> React a
withContext ctx = local (TM.insert ctx)

useContext :: Typeable a => React (Maybe a)
useContext = asks TM.lookup

useContextWithDefault :: Typeable a => a -> React a
useContextWithDefault def = do
  fromMaybe def <$> asks TM.lookup

-- | State
useState :: forall s. Typeable s => s -> React (s, s -> IO ())
useState def = do
  StateMap stateMap <- fromJust <$> useContext
  s <- React (liftIO (fromMaybe def . TM.lookup <$> readTVarIO stateMap))
  pure (s, setState stateMap)
  where
    setState :: TVar TM.TMap -> s -> IO ()
    setState var s = atomically (modifyTVar' var (TM.insert s))

-- | Effect
useEffect :: IO () -> React ()
useEffect = void . React . liftIO . forkIO

render :: Component props -> props -> IO ()
render (Component renderComponent) props = do
  vty <- Vty.mkVty Vty.defaultConfig
  stateMap <- newTVarIO mempty
  pic <- renderComponent props
    & withContext (StateMap stateMap)
    & runReact
    & flip runReaderT mempty
  Vty.update vty pic
  getLine
  Vty.shutdown vty

renderText :: TL.Text -> React Vty.Image
renderText = pure . Vty.text Vty.defAttr
