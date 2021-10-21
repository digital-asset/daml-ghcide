-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Development.IDE.LSP.Server
  ( ReactorMessage(..)
  , ReactorChan
  , ServerM
  , requestHandler
  , notificationHandler
  ) where

import qualified Data.Text as T
import Language.LSP.Server (LspM, Handlers)
import Language.LSP.Types
import qualified Language.LSP.Server as LSP
import Development.IDE.Core.Shake
import Development.IDE.Types.Logger
import UnliftIO.Chan
import Control.Exception
import Control.Monad.Reader

data ReactorMessage
  = ReactorNotification (IO ())
  | ReactorRequest SomeLspId (IO ()) (ResponseError -> IO ())

instance Show ReactorMessage where
    show (ReactorNotification _) = "notification"
    show (ReactorRequest id _ _) = "request:" <> show id

type ReactorChan = Chan ReactorMessage
type ServerM c = ReaderT (ReactorChan, IdeState) (LspM c)

requestHandler
  :: forall (m :: Method 'FromClient 'Request) c.
     SMethod m
  -> (IdeState -> MessageParams m -> LspM c (Either ResponseError (ResponseResult m)))
  -> Handlers (ServerM c)
requestHandler m k = LSP.requestHandler m $ \RequestMessage{_method,_id,_params} resp -> do
  st@(chan,ide) <- ask
  env <- LSP.getLspEnv
  let resp' = flip runReaderT st . resp
  liftIO $ logDebug (ideLogger ide) (T.pack $ "Writing request to chan " <> show _method <> ":" <> show _id)
  liftIO $ writeChan chan (ReactorRequest (SomeLspId _id) (LSP.runLspT env $ resp' =<< k ide _params) (LSP.runLspT env . resp' . Left))
      `catch` (\(e :: SomeException) -> do
                   logDebug (ideLogger ide) $ T.pack  $"Request failed with exception " <> show e <> ": " <> show _method <> ":" <> show _id
                   throwIO e)
  liftIO $ logDebug (ideLogger ide) (T.pack $ "Wrote request to chan " <> show _method <> ":" <> show _id)

notificationHandler
  :: forall (m :: Method 'FromClient 'Notification) c.
     SMethod m
  -> (IdeState -> MessageParams m -> LspM c ())
  -> Handlers (ServerM c)
notificationHandler m k = LSP.notificationHandler m $ \NotificationMessage{_params,_method}-> do
  (chan,ide) <- ask
  env <- LSP.getLspEnv
  writeChan chan $ ReactorNotification (LSP.runLspT env $ k ide _params)
