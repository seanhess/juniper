{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

import Control.Concurrent.STM (newTVar, atomically)
import Control.Monad.IO.Class (MonadIO)
-- import Debug.Trace (traceM)
import Data.String.Conversions (cs)
import Data.Text as Text (stripPrefix, intercalate, Text)
import Data.Text.IO as Text (readFile)
import qualified Data.Text.Lazy as TL
import Data.Map as Map (fromList, toList, keys)
import Web.Scotty as Scotty
import Network.Wai.Middleware.Static (staticWithOptions, defaultOptions)
import Network.Wai (Request, pathInfo, rawPathInfo, Application)
import Lucid (renderBS, Html)
import Lucid.Html5
-- import Counter (view, Model(..), load, update, view)
-- import App (resolve)
import qualified Page.Counter as Counter
import qualified Page.Signup as Signup
import qualified Page.About as About
import qualified Page.Todo as Todo
import qualified Page.Focus as Focus
import Page.Todo (Todo(..))
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.Function ((&))


import Control.Monad.State.Lazy (StateT, execStateT)
import Text.Read (readMaybe)

import Network.HTTP.Types.URI (renderSimpleQuery)

import Juniper.Runtime (Response(..), runAction)
import Juniper.Router (parsePath)
import Juniper.Web (page, lucid, static, handle, render, document, Render(..))


-- TODO back button doesn't work: history.onpopstate? Just call it again with the current url. The url is updating
-- TODO Example: username / password validation
-- TODO Example: tab navigation
-- TODO Example: React client-side component (date picker? Not sure what it would be)

-- TODO VDOM: See below, on HTML-REACT-PARSER. Render HTML, parse client-side, convert to a react component
-- TODO better serialization of actions: use `replace` from html-react-parser

-- TODO update url via header
-- TODO handle empty body -> load only


-- HTML-REACT-PARSER
-- =========================================================
-- https://github.com/remarkablemark/html-react-parser
-- https://www.npmjs.com/package/html-react-parser - let's you swap out certain elements with components, cool.
-- So you should be able to drop in react components and have it work!
-- I can put fancy things in (NOT JAVASCRIPT) and my component can replace them with working coolness
-- XSS - i need to escape the  rendered input on the server
-- Lucid already escapes things! So <script> with give you: &lt;script:gt;. It even escapes quotes. Sick. How does it still work??
-- Just test an XSS attack and see if you can get it to work



-- VDOM =============
-- Virtual Dom Javascript library - looks unmaintained
-- React - why not communicate directly to react? we could probably create view code. Makes embedding other react components easy. Makes people feel happy
-- Elm - probably impossible without reproducing the views

-- <MyButton color="blue" shadowSize={2}>
  -- Click Me
-- </MyButton>
-- React.createElement(
  -- MyButton,
  -- {color: 'blue', shadowSize: 2},
  -- 'Click Me'
-- )
-- <div className="sidebar" />
-- React.createElement(
  -- 'div',
  -- {className: 'sidebar'}
-- )





start :: IO ()
start = do

  -- load embedded js
  todos <- atomically $ newTVar [Todo "Test Item" False]
  let cfg = Render True toDocument

  scotty 3030 $ do
    -- delay to simulate real-world conditions
    -- middleware (delay 500)
    middleware $ staticWithOptions defaultOptions

    page "/app/counter" $ do
      handle cfg Counter.page

    page "/app/signup" $ do
      handle cfg Signup.page

    page "/app/focus" $ do
      handle cfg Focus.page

    page "/app/todo/:n" $ do
      n <- param "n" :: ActionM Int
      -- liftIO $ print n
      handle cfg $ Todo.page todos

    -- if you use "lucid" it doesn't work
    get "/app/about" $
      static $ About.view

    get "/hello/:name" $ do
      name <- param "name"
      html $ mconcat ["Hello: ", name]

    page "/test/:message" $ do
      m <- param "message" :: ActionM TL.Text
      html $ "<div id='container'><p>"<> m <>"</p><input type='text'/><p>Hello!</p><button data-click='Action 3'>PRESS</button></div>"


toDocument :: Html () -> Html ()
toDocument = document "Example" $ do
  link_ [type_ "text/css", rel_ "stylesheet", href_ "/example/example.css"]


delay :: Int -> Application -> Application
delay d application req respond = do
  threadDelay (d * 1000)
  application req respond



