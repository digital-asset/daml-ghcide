-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Development.IDE.Test
  ( Cursor
  , cursorPosition
  , requireDiagnostic
  , expectDiagnostics
  , expectNoMoreDiagnostics
  ) where

import Control.Applicative.Combinators
import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Language.LSP.Test hiding (message)
import qualified Language.LSP.Test as LspTest
import Language.LSP.Types (Diagnostic (..), DiagnosticSeverity (..), List (..), Position (..), SMethod (..), toNormalizedUri)
import qualified Language.LSP.Types.Lens as Lsp
import System.Time.Extra
import Test.Tasty.HUnit


-- | (0-based line number, 0-based column number)
type Cursor = (Int, Int)

cursorPosition :: Cursor -> Position
cursorPosition (line,  col) =
    Position
        (fromIntegral line)
        (fromIntegral col)

requireDiagnostic :: List Diagnostic -> (DiagnosticSeverity, Cursor, T.Text) -> Assertion
requireDiagnostic actuals expected@(severity, cursor, expectedMsg) = do
    unless (any match actuals) $
        assertFailure $
            "Could not find " <> show expected <>
            " in " <> show actuals
  where
    match :: Diagnostic -> Bool
    match d =
        Just severity == _severity d
        && cursorPosition cursor == d ^. Lsp.range . Lsp.start
        && standardizeQuotes (T.toLower expectedMsg) `T.isInfixOf`
           standardizeQuotes (T.toLower $ d ^. Lsp.message)

-- |wait for @timeout@ seconds and report an assertion failure
-- if any diagnostic messages arrive in that period
expectNoMoreDiagnostics :: Seconds -> Session ()
expectNoMoreDiagnostics timeout = do
    -- Give any further diagnostic messages time to arrive.
    liftIO $ sleep timeout
    -- Send a dummy message to provoke a response from the server.
    -- This guarantees that we have at least one message to
    -- process, so message won't block or timeout.
    let cm = SCustomMethod "non-existent-method"
    i <- sendRequest cm Aeson.Null
    go cm i
  where
    go cm i = handleMessages
      where
        handleMessages = handleDiagnostic <|> (void $ responseForId cm i) <|> ignoreOthers
        ignoreOthers = void anyMessage >> handleMessages
    handleDiagnostic = do
        diagsNot <- LspTest.message STextDocumentPublishDiagnostics
        let fileUri = diagsNot ^. Lsp.params . Lsp.uri
            actual = diagsNot ^. Lsp.params . Lsp.diagnostics
        liftIO $ assertFailure $
            "Got unexpected diagnostics for " <> show fileUri <>
            " got " <> show actual

expectDiagnostics :: [(FilePath, [(DiagnosticSeverity, Cursor, T.Text)])] -> Session ()
expectDiagnostics expected = do
    expected' <- Map.fromListWith (<>) <$> traverseOf (traverse . _1) (fmap toNormalizedUri . getDocUri) expected
    go expected'
    where
        go m
            | Map.null m = pure ()
            | otherwise = do
                  diagsNot <- skipManyTill anyMessage (LspTest.message STextDocumentPublishDiagnostics)
                  let fileUri = diagsNot ^. Lsp.params . Lsp.uri
                  case Map.lookup (diagsNot ^. Lsp.params . Lsp.uri . to toNormalizedUri) m of
                      Nothing -> do
                          let actual = diagsNot ^. Lsp.params . Lsp.diagnostics
                          liftIO $ assertFailure $
                              "Got diagnostics for " <> show fileUri <>
                              " but only expected diagnostics for " <> show (Map.keys m) <>
                              " got " <> show actual
                      Just expected -> do
                          let actual = diagsNot ^. Lsp.params . Lsp.diagnostics
                          liftIO $ mapM_ (requireDiagnostic actual) expected
                          liftIO $ unless (length expected == length actual) $
                              assertFailure $
                              "Incorrect number of diagnostics for " <> show fileUri <>
                              ", expected " <> show expected <>
                              " but got " <> show actual
                          go $ Map.delete (diagsNot ^. Lsp.params . Lsp.uri . to toNormalizedUri) m

standardizeQuotes :: T.Text -> T.Text
standardizeQuotes msg = let
        repl '‘' = '\''
        repl '’' = '\''
        repl '`' = '\''
        repl  c   = c
    in  T.map repl msg
