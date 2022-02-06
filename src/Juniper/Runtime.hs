{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Juniper.Runtime where


import Juniper.Page (Page(..), PageAction(..))

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.ByteString.Lazy (ByteString)
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Lazy as TL (Text)
import GHC.Generics (Generic)
import Lucid (Html, renderBS)
import Text.Read (readMaybe)
import Data.Map (Map, (!?))
import Data.Maybe (mapMaybe)
import Data.Function ((&))
import Control.Monad (foldM)



data Message = Message
  { action :: Text
  , url :: Text
  } deriving (Show, Eq, Generic)
instance FromJSON Message


data Response params = Response
  { resView :: Html ()
  , resParams :: params
  } deriving (Show, Generic)



-- class Page model params where
--   toParams :: model -> params
--   loadPage :: (MonadIO m, MonadFail m) => params -> m model






-- One of the actions could be Load
data Command action
  = Init
  | Submit
  | Update action


-- type View model = (model -> Html ())
-- type Update model action m = (action -> Model -> m Model)
--
--


-- | Load the page from route params, then apply the action
runAction
  :: forall m model params action. (Monad m, PageAction action)
  => Page params model action m
  -> Maybe params
  -> [Command action]
  -> m (Response params)
runAction (Page params load update view) ps cmds = do

  -- load the initial model from the parameters
  m <- load ps

  -- run either a load or an action
  m' <- foldM (runCommand update) m cmds

  -- respond
  pure $ Response (view m') (params m')


runCommand :: (Monad m) => (action -> model -> m model) -> model -> Command action -> m model
runCommand update m cmd =
  case cmd of
    Init -> pure m
    Submit -> pure m
    Update a -> update a m




commands :: (MonadFail m, PageAction action) => ByteString -> m [Command action]
commands "" = pure [Init]
commands body = do
  mapM parseCommand $ Text.splitOn "\n" $ cs body


parseCommand :: (MonadFail m, PageAction action) => Text -> m (Command action)
parseCommand "|Submit|" = pure Submit
parseCommand t =
  case readAction (cs t) of
    Just a -> pure $ Update a
    Nothing -> fail $ "Could not parse action: " <> cs t

