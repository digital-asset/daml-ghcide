-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DataKinds #-}

-- | Display information on hover.
module Development.IDE.LSP.HoverDefinition
    (setIdeHandlers) where

import           Development.IDE.Core.Rules
import           Development.IDE.Core.Service
import           Development.IDE.LSP.Server
import           Development.IDE.LSP.Outline
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.Shake
import qualified Language.LSP.Server       as LSP
import           Language.LSP.Types

import qualified Data.Text as T

gotoDefinition :: IdeState -> TextDocumentPositionParams -> LSP.LspM c (Either ResponseError (ResponseResult 'TextDocumentDefinition))
hover          :: IdeState -> TextDocumentPositionParams -> LSP.LspM c (Either ResponseError (Maybe Hover))
gotoDefinition = request "Definition" getDefinition (InR $ InL $ List []) (\l -> InR $ InL $ List [l])
hover          = request "Hover"      getAtPoint     Nothing      foundHover

foundHover :: (Maybe Range, [T.Text]) -> Maybe Hover
foundHover (mbRange, contents) =
  Just $ Hover (HoverContents $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator contents) mbRange

setIdeHandlers :: LSP.Handlers (ServerM c)
setIdeHandlers = mconcat
  [ requestHandler STextDocumentDefinition $ \ide DefinitionParams{..} ->
      gotoDefinition ide TextDocumentPositionParams{..}
  , requestHandler STextDocumentHover $ \ide HoverParams{..} ->
      hover ide TextDocumentPositionParams{..}
  , requestHandler STextDocumentDocumentSymbol moduleOutline
  ]

-- | Respond to and log a hover or go-to-definition request
request
  :: T.Text
  -> (NormalizedFilePath -> Position -> Action (Maybe a))
  -> b
  -> (a -> b)
  -> IdeState
  -> TextDocumentPositionParams
  -> LSP.LspM c (Either ResponseError b)
request label getResults notFound found ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = liftIO $ do
    mbResult <- case uriToFilePath' uri of
        Just path -> logAndRunRequest label getResults ide pos path
        Nothing   -> pure Nothing
    pure $ Right $ maybe notFound found mbResult

logAndRunRequest :: T.Text -> (NormalizedFilePath -> Position -> Action b) -> IdeState -> Position -> String -> IO b
logAndRunRequest label getResults ide pos path = do
  let filePath = toNormalizedFilePath' path
  logInfo (ideLogger ide) $
    label <> " request at position " <> T.pack (showPosition pos) <>
    " in file: " <> T.pack path
  runAction ide $ getResults filePath pos
