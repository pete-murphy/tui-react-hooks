{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Control.Monad.Reader
import Data.Maybe
import qualified Data.TMap as TM
import Data.Typeable
import qualified Graphics.Vty as Vty
import qualified Data.Text.Lazy as TL

newtype React a = React
  { runReact :: ReaderT TM.TMap IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader TM.TMap)

newtype Component props = Component
  { runComponent :: props -> React Vty.Picture
  }

withContext :: Typeable ctx => ctx -> React a -> React a
withContext ctx = local (TM.insert ctx)

useContext :: Typeable a => React (Maybe a)
useContext = do
  asks TM.lookup

useContextWithDefault :: Typeable a => a -> React a
useContextWithDefault def = do
  fromMaybe def <$> asks TM.lookup

render :: Component props -> props -> IO ()
render (Component renderComponent) props = do
  vty <- Vty.mkVty Vty.defaultConfig
  pic <- flip runReaderT mempty (runReact (renderComponent props))
  Vty.update vty pic
  getLine
  Vty.shutdown vty

renderText :: TL.Text -> React Vty.Image
renderText = pure . Vty.text Vty.defAttr
