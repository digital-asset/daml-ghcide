-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}

module Development.IDE.LSP.Notifications
    ( setHandlersNotifications
    ) where

import           Language.LSP.Types
import qualified Language.LSP.Types       as LSP

import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.Service
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.IDE.Plugin

import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Data.Foldable                    as F
import           Data.Maybe
import qualified Data.HashSet                     as S
import qualified Data.Text                        as Text

import           Development.IDE.Core.FileStore   (setSomethingModified)
import           Development.IDE.Core.FileExists  (modifyFileExists)
import           Development.IDE.Core.OfInterest


whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . toNormalizedFilePath'

setHandlersNotifications :: Plugin c
setHandlersNotifications = Plugin
    { pluginRules = mempty
    , pluginCommands = mempty
    , pluginHandlers = mempty
    , pluginNotificationHandlers = mconcat
        [pluginNotificationHandler LSP.STextDocumentDidOpen $
            \ide (DidOpenTextDocumentParams TextDocumentItem{_uri,_version}) -> liftIO $ do
                updatePositionMapping ide (VersionedTextDocumentIdentifier _uri (Just _version)) (List [])
                whenUriFile _uri $ \file -> do
                    modifyFilesOfInterest ide (S.insert file)
                    logInfo (ideLogger ide) $ "Opened text document: " <> getUri _uri

        ,pluginNotificationHandler LSP.STextDocumentDidChange $
            \ide (DidChangeTextDocumentParams identifier@VersionedTextDocumentIdentifier{_uri} changes) -> liftIO $ do
                updatePositionMapping ide identifier changes
                setSomethingModified ide
                logInfo (ideLogger ide) $ "Modified text document: " <> getUri _uri

        ,pluginNotificationHandler LSP.STextDocumentDidSave $
            \ide (DidSaveTextDocumentParams TextDocumentIdentifier{_uri} _) -> liftIO $ do
                setSomethingModified ide
                logInfo (ideLogger ide) $ "Saved text document: " <> getUri _uri

        ,pluginNotificationHandler LSP.STextDocumentDidClose $
            \ide (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> liftIO $ do
                whenUriFile _uri $ \file -> do
                    modifyFilesOfInterest ide (S.delete file)
                    logInfo (ideLogger ide) $ "Closed text document: " <> getUri _uri

        ,pluginNotificationHandler LSP.SWorkspaceDidChangeWatchedFiles $
            \ide (DidChangeWatchedFilesParams fileEvents) -> liftIO $ do
                let events =
                        mapMaybe
                            (\(FileEvent uri ev) ->
                                (, ev /= FcDeleted) . toNormalizedFilePath'
                                <$> LSP.uriToFilePath uri
                            )
                            ( F.toList fileEvents )
                let msg = Text.pack $ show events
                logInfo (ideLogger ide) $ "Files created or deleted: " <> msg
                modifyFileExists ide events
                setSomethingModified ide

        ,pluginNotificationHandler LSP.SWorkspaceDidChangeWorkspaceFolders $
            \ide (DidChangeWorkspaceFoldersParams events) -> liftIO $ do
                let add       = S.union
                    substract = flip S.difference
                modifyWorkspaceFolders ide
                  $ add       (foldMap (S.singleton . parseWorkspaceFolder) (_added   events))
                  . substract (foldMap (S.singleton . parseWorkspaceFolder) (_removed events))
        ]
    }
