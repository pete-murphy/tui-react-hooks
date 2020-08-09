{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Text.Lazy as TL
import GHC.TypeNats
import qualified Graphics.Vty as Vty
import Lib

helloWorld :: Component ()
helloWorld = Component \_ ->
  Vty.picForImage <$> renderText "Hello, world"

newtype Name (p :: Nat) = Name TL.Text

sayHello :: Component ()
sayHello = Component \_ -> do
  Name n0 <- useContextWithDefault (Name @1 "stranger")
  Name n1 <- useContextWithDefault (Name @2 "stranger")
  Vty.picForImage <$> renderText ("Hello, " <> n0 <> " and " <> n1)

sayHelloTo :: Component TL.Text
sayHelloTo = Component \name -> do
  withContext (Name @1 name)
    (withContext (Name @2 "Stacy") (runComponent sayHello ()))

timer :: Component ()
timer = Component \_ -> do
  (counter, setCounter) <- useState (0 :: Int)
  Vty.picForImage <$> renderText ("Counter: " <> TL.pack (show counter))

main :: IO ()
main = render timer ()
