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

import Language.LSP.Server (LspM, Handlers)
import Language.LSP.Types
import qualified Language.LSP.Server as LSP
import Development.IDE.Core.Shake
import UnliftIO.Chan
import Control.Monad.Reader

data ReactorMessage
  = ReactorNotification (IO ())
  | ReactorRequest SomeLspId (IO ()) (ResponseError -> IO ())

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
  writeChan chan $ ReactorRequest (SomeLspId _id) (LSP.runLspT env $ resp' =<< k ide _params) (LSP.runLspT env . resp' . Left)

notificationHandler
  :: forall (m :: Method 'FromClient 'Notification) c.
     SMethod m
  -> (IdeState -> MessageParams m -> LspM c ())
  -> Handlers (ServerM c)
notificationHandler m k = LSP.notificationHandler m $ \NotificationMessage{_params,_method}-> do
  (chan,ide) <- ask
  env <- LSP.getLspEnv
  writeChan chan $ ReactorNotification (LSP.runLspT env $ k ide _params)
