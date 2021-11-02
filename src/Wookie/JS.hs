{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Wookie.JS where
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

scripts :: ByteString
scripts = build <> "\n" <> run

-- | Embed built javascript into file via Data.FileEmbed. Must be recompiled via node to work
build :: ByteString
build = $(embedFile "edom/build.js")

run :: ByteString
run = $(embedFile "edom/run.js")
