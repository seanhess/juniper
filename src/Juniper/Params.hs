{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Juniper.Params where

import Juniper.Prelude
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)



-- TODO use generics directly instead of JSON
-- does it always serialize to a querystring?
-- what if you wanted to use paths?
class ToParams params where
  encode :: params -> Text
  decode :: Text -> Maybe params

  default encode :: Generic params => params -> Text
  encode _ = ""

  default decode :: Generic params => Text -> Maybe params
  decode _ = Nothing

-- instance ToParams Day where
--   encode = cs . formatTime defaultTimeLocale "%Y-%m-%d"
--   decode = parseTimeM True defaultTimeLocale "%Y-%m-%d" . cs

instance ToParams ()

-- instance (ToJSON a, ToJSON b, FromJSON a, FromJSON b) => ToParams (a, b)
-- instance (ToJSON a, ToJSON b, ToJSON c, FromJSON a, FromJSON b, FromJSON c) => ToParams (a, b, c)
-- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON a, FromJSON b, FromJSON c, FromJSON d) => ToParams (a, b, c, d)



