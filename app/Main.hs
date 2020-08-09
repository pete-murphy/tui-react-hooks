{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Graphics.Vty as Vty
import Lib
import qualified Data.Text.Lazy as TL

helloWorld :: Component ()
helloWorld = Component \_ ->
  Vty.picForImage <$> renderText "Hello, world"

newtype Name = Name TL.Text

sayHello :: Component ()
sayHello = Component \_ -> do
  Name name <- useContextWithDefault (Name "stranger")
  Vty.picForImage <$> renderText ("Hello, " <> name)

sayHelloTo :: Component TL.Text
sayHelloTo = Component \name -> do
  withContext (Name name) (runComponent sayHello ())

main :: IO ()
main = render sayHelloTo "Bob"
