-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

module Main (main) where

import Control.Applicative.Combinators
import Control.Exception (catch)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Foldable
import Data.List.Extra
import Data.Rope.UTF16 (Rope)
import qualified Data.Rope.UTF16 as Rope
import Development.IDE.Core.PositionMapping (fromCurrent, toCurrent)
import Development.IDE.GHC.Util
import qualified Data.Text as T
import Development.IDE.Spans.Common
import Development.IDE.Test
import Development.IDE.Test.Runfiles
import Development.IDE.Types.Location
import Language.LSP.Test
import Language.LSP.Types (
    CodeAction (..),
    CodeLens (..),
    CodeLensOptions (..),
    Command (..),
    CompletionDoc (..),
    CompletionItem (..),
    CompletionItemKind (..),
    CompletionOptions (..),
    CustomMessage (..),
    Diagnostic (..),
    DiagnosticSeverity (..),
    DidChangeWatchedFilesRegistrationOptions (..),
    DidOpenTextDocumentParams (..),
    DocumentSymbol (..),
    ExecuteCommandOptions (..),
    FromServerMessage,
    FromServerMessage' (..),
    Hover (..),
    HoverContents (..),
    InitializeResult (..),
    InsertTextFormat (..),
    List (..),
    LocationLink (..),
    MarkupContent (..),
    Method (..),
    NotificationMessage (..),
    ProgressParams (..),
    PublishDiagnosticsParams (..),
    Registration (..),
    RegistrationParams (..),
    RequestMessage (..),
    ResponseMessage (..),
    SaveOptions (..),
    SMethod (..),
    SomeProgressParams (..),
    SomeRegistration (..),
    SServerMethod,
    SymbolKind (..),
    TextDocumentContentChangeEvent (..),
    TextDocumentIdentifier (..),
    TextDocumentItem (..),
    TextDocumentSyncKind (..),
    TextDocumentSyncOptions (..),
    UInt,
    type (|?) (..),
    filePathToUri,
    toEither,
    uriToFilePath,
  )
import Language.LSP.Types.Capabilities
import Language.LSP.VFS (applyChange)
import Network.URI
import System.Environment.Blank (setEnv)
import System.FilePath
import System.IO.Extra
import System.Directory
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Maybe

waitForProgressBegin :: Session ()
waitForProgressBegin = skipManyTill anyMessage $ satisfyMaybe $ \case
  FromServerMess SProgress (NotificationMessage _ _ (ProgressParams _ (Begin _))) -> Just ()
  _ -> Nothing

waitForProgressDone :: Session ()
waitForProgressDone = skipManyTill anyMessage $ satisfyMaybe $ \case
  FromServerMess SProgress (NotificationMessage _ _ (ProgressParams _ (End _))) -> Just ()
  _ -> Nothing

main :: IO ()
main = do
  -- We mess with env vars so run single-threaded.
  setEnv "TASTY_NUM_THREADS" "1" True
  defaultMainWithRerun $ testGroup "ghcide"
    [ testSession "open close" $ do
        doc <- createDoc "Testing.hs" "haskell" ""
        void (skipManyTill anyMessage (message SWindowWorkDoneProgressCreate))
        waitForProgressBegin
        closeDoc doc
        waitForProgressDone
    , initializeResponseTests
    , completionTests
    , cppTests
    , diagnosticTests
    , codeActionTests
    , codeLensesTests
    , outlineTests
    , findDefinitionAndHoverTests
    , pluginTests
    , preprocessorTests
    , thTests
    , safeTests
    , unitTests
    , haddockTests
    , positionMappingTests
    , watchedFilesTests
    , cradleTests
    ]

initializeResponseTests :: TestTree
initializeResponseTests = withResource acquire release tests where

  -- these tests document and monitor the evolution of the
  -- capabilities announced by the server in the initialize
  -- response. Currently the server advertises almost no capabilities
  -- at all, in some cases failing to announce capabilities that it
  -- actually does provide! Hopefully this will change ...
  tests :: IO (ResponseMessage 'Initialize) -> TestTree
  tests getInitializeResponse =
    testGroup "initialize response capabilities"
    [ chk "   text doc sync"             _textDocumentSync  tds
    , chk "   hover"                         _hoverProvider (Just $ InL True)
    , chk "   completion"               _completionProvider (Just $ CompletionOptions Nothing (Just ["."]) Nothing (Just False))
    , chk "NO signature help"        _signatureHelpProvider  Nothing
    , chk "   goto definition"          _definitionProvider (Just $ InL True)
    , chk "NO goto type definition" _typeDefinitionProvider (Just $ InL False)
    , chk "NO goto implementation"  _implementationProvider (Just $ InL False)
    , chk "NO find references"          _referencesProvider  (Just $ InL False)
    , chk "NO doc highlight"     _documentHighlightProvider  (Just $ InL False)
    , chk "   doc symbol"           _documentSymbolProvider  (Just $ InL True)
    , chk "NO workspace symbol"    _workspaceSymbolProvider  (Just False)
    , chk "   code action"             _codeActionProvider $ Just $ InL True
    , chk "   code lens"                 _codeLensProvider $ Just $ CodeLensOptions (Just False) (Just False)
    , chk "NO doc formatting"   _documentFormattingProvider  (Just $ InL False)
    , chk "NO doc range formatting"
                           _documentRangeFormattingProvider  (Just $ InL False)
    , chk "NO doc formatting on typing"
                          _documentOnTypeFormattingProvider  Nothing
    , chk "NO renaming"                     _renameProvider (Just $ InL False)
    , chk "NO doc link"               _documentLinkProvider  Nothing
    , chk "NO color"                         _colorProvider (Just $ InL False)
    , chk "NO folding range"          _foldingRangeProvider (Just $ InL False)
    -- TODO fixme
    , che "   execute command"      _executeCommandProvider ["typesignature.add"]
    , chk "   workspace"                         _workspace (Just $ WorkspaceServerCapabilities (Just WorkspaceFoldersServerCapabilities{_supported = Just True, _changeNotifications = Just ( InR True )}))
    , chk "NO experimental"                   _experimental  Nothing
    ] where

      tds = Just (InL (TextDocumentSyncOptions
                              { _openClose = Just True
                              , _change    = Just TdSyncIncremental
                              , _willSave  = Nothing
                              , _willSaveWaitUntil = Nothing
                              , _save = Just (InR $ SaveOptions {_includeText = Nothing})}))

      chk :: (Eq a, Show a) => TestName -> (ServerCapabilities -> a) -> a -> TestTree
      chk title getActual expected =
        testCase title $ getInitializeResponse >>= \ir -> expected @=? (getActual . innerCaps) ir

      che :: TestName -> (ServerCapabilities -> Maybe ExecuteCommandOptions) -> [T.Text] -> TestTree
      che title getActual expected = testCase title doTest
        where
            doTest = do
                ir <- getInitializeResponse
                let Just ExecuteCommandOptions {_commands = List commands} = getActual $ innerCaps ir
                zipWithM_ (\e o -> T.isSuffixOf e o @? show (e,o)) expected commands

  innerCaps :: ResponseMessage 'Initialize -> ServerCapabilities
  innerCaps (ResponseMessage _ _ (Right (InitializeResult c _))) = c
  innerCaps (ResponseMessage _ _ (Left _)) = error "Initialization error"

  acquire :: IO (ResponseMessage 'Initialize)
  acquire = run initializeResponse

  release :: ResponseMessage 'Initialize -> IO ()
  release = const $ pure ()


diagnosticTests :: TestTree
diagnosticTests = testGroup "diagnostics"
  [ testSessionWait "fix syntax error" $ do
      let content = T.unlines [ "module Testing wher" ]
      doc <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "parse error")])]
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 15) (Position 0 19))
            , _rangeLength = Nothing
            , _text = "where"
            }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [])]
  , testSessionWait "introduce syntax error" $ do
      let content = T.unlines [ "module Testing where" ]
      doc <- createDoc "Testing.hs" "haskell" content
      void (skipManyTill anyMessage (message SWindowWorkDoneProgressCreate))
      waitForProgressBegin
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 15) (Position 0 18))
            , _rangeLength = Nothing
            , _text = "wher"
            }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "parse error")])]
  , testSessionWait "variable not in scope" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int -> Int"
            , "foo a b = a + ab"
            , "bar :: Int -> Int -> Int"
            , "bar a b = cd + b"
            ]
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [ (DsError, (2, 14), "Variable not in scope: ab")
            , (DsError, (4, 10), "Variable not in scope: cd")
            ]
          )
        ]
  , testSessionWait "type error" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> String -> Int"
            , "foo a b = a + b"
            ]
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [(DsError, (2, 14), "Couldn't match type '[Char]' with 'Int'")]
          )
        ]
  , testSessionWait "typed hole" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> String"
            , "foo a = _ a"
            ]
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [(DsError, (2, 8), "Found hole: _ :: Int -> String")]
          )
        ]

  , testGroup "deferral" $
    let sourceA a = T.unlines
          [ "module A where"
          , "a :: Int"
          , "a = " <> a]
        sourceB = T.unlines
          [ "module B where"
          , "import A"
          , "b :: Float"
          , "b = True"]
        bMessage = "Couldn't match expected type 'Float' with actual type 'Bool'"
        expectedDs aMessage =
          [ ("A.hs", [(DsError, (2,4), aMessage)])
          , ("B.hs", [(DsError, (3,4), bMessage)])]
        deferralTest title binding msg = testSessionWait title $ do
          _ <- createDoc "A.hs" "haskell" $ sourceA binding
          _ <- createDoc "B.hs" "haskell"   sourceB
          expectDiagnostics $ expectedDs msg
    in
    [ deferralTest "type error"          "True"    "Couldn't match expected type"
    , deferralTest "typed hole"          "_"       "Found hole"
    , deferralTest "out of scope var"    "unbound" "Variable not in scope"
    , deferralTest "message shows error" "True"    "A.hs:3:5: error:"
    ]

  , testSessionWait "remove required module" $ do
      let contentA = T.unlines [ "module ModuleA where" ]
      docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 0) (Position 0 20))
            , _rangeLength = Nothing
            , _text = ""
            }
      changeDoc docA [change]
      expectDiagnostics [("ModuleB.hs", [(DsError, (1, 0), "Could not find module")])]
  , testSessionWait "add missing module" $ do
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      expectDiagnostics [("ModuleB.hs", [(DsError, (1, 7), "Could not find module")])]
      let contentA = T.unlines [ "module ModuleA where" ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      expectDiagnostics [("ModuleB.hs", [])]
  , testSessionWait "add missing module (non workspace)" $ do
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- createDoc "/tmp/ModuleB.hs" "haskell" contentB
      expectDiagnostics [("/tmp/ModuleB.hs", [(DsError, (1, 7), "Could not find module")])]
      let contentA = T.unlines [ "module ModuleA where" ]
      _ <- createDoc "/tmp/ModuleA.hs" "haskell" contentA
      expectDiagnostics [("/tmp/ModuleB.hs", [])]
  , testSessionWait "cyclic module dependency" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import ModuleB"
            ]
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      expectDiagnostics
        [ ( "ModuleA.hs"
          , [(DsError, (1, 7), "Cyclic module dependency between ModuleA, ModuleB")]
          )
        , ( "ModuleB.hs"
          , [(DsError, (1, 7), "Cyclic module dependency between ModuleA, ModuleB")]
          )
        ]
  , testSessionWait "cyclic module dependency with hs-boot" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import {-# SOURCE #-} ModuleB"
            ]
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      let contentBboot = T.unlines
            [ "module ModuleB where"
            ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- createDoc "ModuleB.hs-boot" "haskell" contentBboot
      expectDiagnostics []
  , testSessionWait "correct reference used with hs-boot" $ do
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import {-# SOURCE #-} ModuleA"
            ]
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import ModuleB"
            , "x = 5"
            ]
      let contentAboot = T.unlines
            [ "module ModuleA where"
            ]
      let contentC = T.unlines
            [ "module ModuleC where"
            , "import ModuleA"
            -- this reference will fail if it gets incorrectly
            -- resolved to the hs-boot file
            , "y = x"
            ]
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleA.hs-boot" "haskell" contentAboot
      _ <- createDoc "ModuleC.hs" "haskell" contentC
      expectDiagnostics []
  , testSessionWait "redundant import" $ do
      let contentA = T.unlines ["module ModuleA where"]
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      expectDiagnostics
        [ ( "ModuleB.hs"
          , [(DsWarning, (2, 0), "The import of 'ModuleA' is redundant")]
          )
        ]
  , testSessionWait "package imports" $ do
      let thisDataListContent = T.unlines
            [ "module Data.List where"
            , "x :: Integer"
            , "x = 123"
            ]
      let mainContent = T.unlines
            [ "{-# LANGUAGE PackageImports #-}"
            , "module Main where"
            , "import qualified \"this\" Data.List as ThisList"
            , "import qualified \"base\" Data.List as BaseList"
            , "useThis = ThisList.x"
            , "useBase = BaseList.map"
            , "wrong1 = ThisList.map"
            , "wrong2 = BaseList.x"
            ]
      _ <- createDoc "Data/List.hs" "haskell" thisDataListContent
      _ <- createDoc "Main.hs" "haskell" mainContent
      expectDiagnostics
        [ ( "Main.hs"
          , [(DsError, (6, 9), "Not in scope: \8216ThisList.map\8217")
            ,(DsError, (7, 9), "Not in scope: \8216BaseList.x\8217")
            ]
          )
        ]
  , testSessionWait "unqualified warnings" $ do
      let fooContent = T.unlines
            [ "{-# OPTIONS_GHC -Wredundant-constraints #-}"
            , "module Foo where"
            , "foo :: Ord a => a -> Int"
            , "foo a = 1"
            ]
      _ <- createDoc "Foo.hs" "haskell" fooContent
      expectDiagnostics
        [ ( "Foo.hs"
      -- The test is to make sure that warnings contain unqualified names
      -- where appropriate. The warning should use an unqualified name 'Ord', not
      -- sometihng like 'GHC.Classes.Ord'. The choice of redundant-constraints to
      -- test this is fairly arbitrary.
          , [(DsWarning, (2, 0), "Redundant constraint: Ord a")
            ]
          )
        ]
    , testSessionWait "lower-case drive" $ do
          let aContent = T.unlines
                [ "module A.A where"
                , "import A.B ()"
                ]
              bContent = T.unlines
                [ "{-# OPTIONS_GHC -Wall #-}"
                , "module A.B where"
                , "import Data.List"
                ]
          uriB <- getDocUri "A/B.hs"
          Just pathB <- pure $ uriToFilePath uriB
          uriB <- pure $
              let (drive, suffix) = splitDrive pathB
              in filePathToUri (joinDrive (lower drive) suffix)
          liftIO $ createDirectoryIfMissing True (takeDirectory pathB)
          liftIO $ writeFileUTF8 pathB $ T.unpack bContent
          uriA <- getDocUri "A/A.hs"
          Just pathA <- pure $ uriToFilePath uriA
          uriA <- pure $
              let (drive, suffix) = splitDrive pathA
              in filePathToUri (joinDrive (lower drive) suffix)
          let itemA = TextDocumentItem uriA "haskell" 0 aContent
          let a = TextDocumentIdentifier uriA
          sendNotification STextDocumentDidOpen (DidOpenTextDocumentParams itemA)
          NotificationMessage{_params = PublishDiagnosticsParams fileUri _ diags} <- skipManyTill anyMessage (message STextDocumentPublishDiagnostics)
          -- Check that if we put a lower-case drive in for A.A
          -- the diagnostics for A.B will also be lower-case.
          liftIO $ fileUri @?= uriB
          let msg = _message (head (toList diags) :: Diagnostic)
          liftIO $ unless ("redundant" `T.isInfixOf` msg) $
              assertFailure ("Expected redundant import but got " <> T.unpack msg)
          closeDoc a
  ]

codeActionTests :: TestTree
codeActionTests = testGroup "code actions"
  [ renameActionTests
  , typeWildCardActionTests
  , removeImportTests
  , extendImportTests
  , suggestImportTests
  , addExtensionTests
  , fixConstructorImportTests
  , importRenameActionTests
  , fillTypedHoleTests
  , addSigActionTests
  , insertNewDefinitionTests
  ]

codeLensesTests :: TestTree
codeLensesTests = testGroup "code lenses"
  [ addSigLensesTests
  ]

watchedFilesTests :: TestTree
watchedFilesTests = testGroup "watched files"
  [ testSession' "workspace files" $ \sessionDir -> do
      liftIO $ writeFile (sessionDir </> "hie.yaml") "cradle: {direct: {arguments: [\"-isrc\"]}}"
      _ <- createDoc "A.hs" "haskell" "{-#LANGUAGE NoImplicitPrelude #-}\nmodule A where\nimport B"
      watchedFileRegs <- getWatchedFilesSubscriptionsUntil STextDocumentPublishDiagnostics

      -- Expect 6 subscriptions (A does not get any because it's VFS):
      --  - /path-to-workspace/B.hs
      --  - /path-to-workspace/B.lhs
      --  - B.hs
      --  - B.lhs
      --  - src/B.hs
      --  - src/B.lhs
      liftIO $ length watchedFileRegs @?= 6

  , testSession' "non workspace file" $ \sessionDir -> do
      liftIO $ writeFile (sessionDir </> "hie.yaml") "cradle: {direct: {arguments: [\"-i/tmp\"]}}"
      _ <- createDoc "A.hs" "haskell" "{-# LANGUAGE NoImplicitPrelude#-}\nmodule A where\nimport B"
      watchedFileRegs <- getWatchedFilesSubscriptionsUntil STextDocumentPublishDiagnostics

      -- Expect 4 subscriptions:
      liftIO $ length watchedFileRegs @?= 4

  -- TODO add a test for didChangeWorkspaceFolder
  ]

renameActionTests :: TestTree
renameActionTests = testGroup "rename actions"
  [ testSession "change to local variable name" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int"
            , "foo argName = argNme"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      action <- findCodeAction doc (Just (Range (Position 2 14) (Position 2 20))) "Replace with ‘argName’"
      executeCodeAction action
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int"
            , "foo argName = argName"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "change to name of imported function" $ do
      let content = T.unlines
            [ "module Testing where"
            , "import Data.Maybe (maybeToList)"
            , "foo :: Maybe a -> [a]"
            , "foo = maybToList"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      action <- findCodeAction doc (Just (Range (Position 3 6) (Position 3 16)))  "Replace with ‘maybeToList’"
      executeCodeAction action
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "import Data.Maybe (maybeToList)"
            , "foo :: Maybe a -> [a]"
            , "foo = maybeToList"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "suggest multiple local variable names" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Char -> Char -> Char -> Char"
            , "foo argument1 argument2 argument3 = argumentX"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      _ <- findCodeActions doc (Just (Range (Position 2 36) (Position 2 45)))
                           ["Replace with ‘argument1’", "Replace with ‘argument2’", "Replace with ‘argument3’"]
      return()
  , testSession "change infix function" $ do
      let content = T.unlines
            [ "module Testing where"
            , "monus :: Int -> Int"
            , "monus x y = max 0 (x - y)"
            , "foo x y = x `monnus` y"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 3 12) (Position 3 20))
      [fixTypo] <- pure [action | InR action@CodeAction{ _title = actionTitle } <- actionsOrCommands, "monus" `T.isInfixOf` actionTitle ]
      executeCodeAction fixTypo
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "monus :: Int -> Int"
            , "monus x y = max 0 (x - y)"
            , "foo x y = x `monus` y"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

typeWildCardActionTests :: TestTree
typeWildCardActionTests = testGroup "type wildcard actions"
  [ testSession "global signature" $ do
      let content = T.unlines
            [ "module Testing where"
            , "func :: _"
            , "func x = x"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 1 1) (Position 1 10))
      let [addSignature] = [action | InR action@CodeAction { _title = actionTitle } <- actionsOrCommands
                                   , "Use type signature" `T.isInfixOf` actionTitle
                           ]
      executeCodeAction addSignature
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "func :: (p -> p)"
            , "func x = x"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "multi-line message" $ do
      let content = T.unlines
            [ "module Testing where"
            , "func :: _"
            , "func x y = x + y"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 1 1) (Position 1 10))
      let [addSignature] = [action | InR action@CodeAction { _title = actionTitle } <- actionsOrCommands
                                    , "Use type signature" `T.isInfixOf` actionTitle
                              ]
      executeCodeAction addSignature
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "func :: (Integer -> Integer -> Integer)"
            , "func x y = x + y"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "local signature" $ do
      let content = T.unlines
            [ "module Testing where"
            , "func :: Int -> Int"
            , "func x ="
            , "  let y :: _"
            , "      y = x * 2"
            , "  in y"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 3 1) (Position 3 10))
      let [addSignature] = [action | InR action@CodeAction { _title = actionTitle } <- actionsOrCommands
                                    , "Use type signature" `T.isInfixOf` actionTitle
                              ]
      executeCodeAction addSignature
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "func :: Int -> Int"
            , "func x ="
            , "  let y :: (Int)"
            , "      y = x * 2"
            , "  in y"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

removeImportTests :: TestTree
removeImportTests = testGroup "remove import actions"
  [ testSession "redundant" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "qualified redundant" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant binding" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "stuffA = False"
            , "stuffB :: Integer"
            , "stuffB = 123"
            , "stuffC = ()"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (stuffA, stuffB, stuffC, stuffA)"
            , "main = print stuffB"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove stuffA, stuffC from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (stuffB)"
            , "main = print stuffB"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant operator" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "a !! b = a"
            , "a <?> b = a"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA as A ((<?>), stuffB, (!!))"
            , "main = print A.stuffB"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
#if MIN_GHC_API_VERSION(8,6,0)
      liftIO $ "Remove !!, <?> from import" @=? actionTitle
#else
      liftIO $ "Remove A.!!, A.<?> from import" @=? actionTitle
#endif
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA as A (stuffB)"
            , "main = print A.stuffB"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant all import" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "data A = A"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (A(..), stuffB)"
            , "main = print stuffB"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove A from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (stuffB)"
            , "main = print stuffB"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant constructor import" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "data D = A | B"
            , "data E = F"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (D(A,B), E(F))"
            , "main = B"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove A, E, F from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (D(B))"
            , "main = B"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

extendImportTests :: TestTree
extendImportTests = testGroup "extend import actions"
  [ testSession "extend single line import with value" $ template
      (T.unlines
            [ "module ModuleA where"
            , "stuffA :: Double"
            , "stuffA = 0.00750"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ])
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA as A (stuffB)"
            , "main = print (stuffA, stuffB)"
            ])
      (Range (Position 2 17) (Position 2 18))
      "Add stuffA to the import list of ModuleA"
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA as A (stuffA, stuffB)"
            , "main = print (stuffA, stuffB)"
            ])
  , testSession "extend single line import with operator" $ template
      (T.unlines
            [ "module ModuleA where"
            , "(.*) :: Integer -> Integer -> Integer"
            , "x .* y = x * y"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ])
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA as A (stuffB)"
            , "main = print (stuffB .* stuffB)"
            ])
      (Range (Position 2 17) (Position 2 18))
      "Add .* to the import list of ModuleA"
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA as A ((.*), stuffB)"
            , "main = print (stuffB .* stuffB)"
            ])
  , testSession "extend single line import with type" $ template
      (T.unlines
            [ "module ModuleA where"
            , "type A = Double"
            ])
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA ()"
            , "b :: A"
            , "b = 0"
            ])
      (Range (Position 2 5) (Position 2 5))
      "Add A to the import list of ModuleA"
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA (A)"
            , "b :: A"
            , "b = 0"
            ])
  ,  (`xfail` "known broken") $ testSession "extend single line import with constructor" $ template
      (T.unlines
            [ "module ModuleA where"
            , "data A = Constructor"
            ])
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA (A)"
            , "b :: A"
            , "b = Constructor"
            ])
      (Range (Position 2 5) (Position 2 5))
      "Add Constructor to the import list of ModuleA"
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA (A(Constructor))"
            , "b :: A"
            , "b = Constructor"
            ])
  , testSession "extend single line qualified import with value" $ template
      (T.unlines
            [ "module ModuleA where"
            , "stuffA :: Double"
            , "stuffA = 0.00750"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ])
      (T.unlines
            [ "module ModuleB where"
            , "import qualified ModuleA as A (stuffB)"
            , "main = print (A.stuffA, A.stuffB)"
            ])
      (Range (Position 2 17) (Position 2 18))
      "Add stuffA to the import list of ModuleA"
      (T.unlines
            [ "module ModuleB where"
            , "import qualified ModuleA as A (stuffA, stuffB)"
            , "main = print (A.stuffA, A.stuffB)"
            ])
  , testSession "extend multi line import with value" $ template
      (T.unlines
            [ "module ModuleA where"
            , "stuffA :: Double"
            , "stuffA = 0.00750"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ])
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA (stuffB"
            , "               )"
            , "main = print (stuffA, stuffB)"
            ])
      (Range (Position 3 17) (Position 3 18))
      "Add stuffA to the import list of ModuleA"
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA (stuffA, stuffB"
            , "               )"
            , "main = print (stuffA, stuffB)"
            ])
  ]
  where
    template contentA contentB range expectedAction expectedContentB = do
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      InR action@CodeAction { _title = actionTitle } : _
                  <- sortOn (\(InR CodeAction{_title=x}) -> x) <$>
                     getCodeActions docB range
      liftIO $ expectedAction @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ expectedContentB @=? contentAfterAction

suggestImportTests :: TestTree
suggestImportTests = testGroup "suggest import actions"
  [ testGroup "Dont want suggestion"
    [ -- extend import
      test False ["Data.List.NonEmpty ()"] "f = nonEmpty" []                "import Data.List.NonEmpty (nonEmpty)"
      -- data constructor
    , test False []                        "f = First"    []                "import Data.Monoid (First)"
      -- internal module
    , test False []         "f :: Typeable a => a"        ["f = undefined"] "import Data.Typeable.Internal (Typeable)"
      -- package not in scope
    , test False []         "f = quickCheck"              []                "import Test.QuickCheck (quickCheck)"
    ]
  , testGroup "want suggestion"
    [ test True []          "f = nonEmpty"                []                "import Data.List.NonEmpty (nonEmpty)"
    , test True []          "f = (:|)"                    []                "import Data.List.NonEmpty (NonEmpty((:|)))"
    , test True []          "f :: Natural"                ["f = undefined"] "import Numeric.Natural (Natural)"
    , test True []          "f :: NonEmpty ()"            ["f = () :| []"]  "import Data.List.NonEmpty (NonEmpty)"
    , test True []          "f = First"                   []                "import Data.Monoid (First(First))"
    , test True []          "f = Endo"                    []                "import Data.Monoid (Endo(Endo))"
    , test True []          "f = Version"                 []                "import Data.Version (Version(Version))"
    , test True []          "f ExitSuccess = ()"          []                "import System.Exit (ExitCode(ExitSuccess))"
    , test True []          "f = AssertionFailed"         []                "import Control.Exception (AssertionFailed(AssertionFailed))"
    , test True ["Prelude"] "f = nonEmpty"                []                "import Data.List.NonEmpty (nonEmpty)"
    , test True []          "f :: Alternative f => f ()"  ["f = undefined"] "import Control.Applicative (Alternative)"
    , test True []          "f = empty"                   []                "import Control.Applicative (Alternative(empty))"
    , test True []          "f = (&)"                     []                "import Data.Function ((&))"
    , test True []          "f = NE.nonEmpty"             []                "import qualified Data.List.NonEmpty as NE"
    , test True []          "f :: Typeable a => a"        ["f = undefined"] "import Data.Typeable (Typeable)"
    , test True []          "f = pack"                    []                "import Data.Text (pack)"
    , test True []          "f :: Text"                   ["f = undefined"] "import Data.Text (Text)"
    ]
  ]
  where
    test wanted imps def other newImp = testSession' (T.unpack def) $ \dir -> do
      let before = T.unlines $ "module A where" : ["import " <> x | x <- imps] ++ def : other
          after  = T.unlines $ "module A where" : ["import " <> x | x <- imps] ++ [newImp] ++ def : other
          cradle = "cradle: {direct: {arguments: [-hide-all-packages, -package, base, -package, text, -package-env, -]}}"
      liftIO $ writeFileUTF8 (dir </> "hie.yaml") cradle
      doc <- createDoc "Test.hs" "haskell" before
      _diags <- waitForDiagnostics
      let defLine = fromIntegral $ length imps + 1
          range = Range (Position defLine 0) (Position defLine maxBound)
      actions <- getCodeActions doc range
      if wanted
         then do
             action <- liftIO $ pickActionWithTitle newImp actions
             executeCodeAction action
             contentAfterAction <- documentContents doc
             liftIO $ after @=? contentAfterAction
          else
              liftIO $ [_title | InR CodeAction{_title} <- actions, _title == newImp ] @?= []


addExtensionTests :: TestTree
addExtensionTests = testGroup "add language extension actions"
  [ testSession "add NamedFieldPuns language extension" $ template
      (T.unlines
            [ "module Module where"
            , ""
            , "data A = A { getA :: Bool }"
            , ""
            , "f :: A -> Bool"
            , "f A { getA } = getA"
            ])
      "Add NamedFieldPuns extension"
      (T.unlines
            [ "{-# LANGUAGE NamedFieldPuns #-}"
            , "module Module where"
            , ""
            , "data A = A { getA :: Bool }"
            , ""
            , "f :: A -> Bool"
            , "f A { getA } = getA"
            ])
  , testSession "add RecordWildCards language extension" $ template
      (T.unlines
            [ "module Module where"
            , ""
            , "data A = A { getA :: Bool }"
            , ""
            , "f :: A -> Bool"
            , "f A { .. } = getA"
            ])
      "Add RecordWildCards extension"
      (T.unlines
            [ "{-# LANGUAGE RecordWildCards #-}"
            , "module Module where"
            , ""
            , "data A = A { getA :: Bool }"
            , ""
            , "f :: A -> Bool"
            , "f A { .. } = getA"
            ])
  ]
    where
      template initialContent expectedAction expectedContents = do
        doc <- createDoc "Module.hs" "haskell" initialContent
        _ <- waitForDiagnostics
        InR action@CodeAction { _title = actionTitle } : _
                    <- sortOn (\(InR CodeAction{_title=x}) -> x) <$>
                       getAllCodeActions doc
        liftIO $ expectedAction @=? actionTitle
        executeCodeAction action
        contentAfterAction <- documentContents doc
        liftIO $ expectedContents @=? contentAfterAction


insertNewDefinitionTests :: TestTree
insertNewDefinitionTests = testGroup "insert new definition actions"
  [ testSession "insert new function definition" $ do
      let txtB =
            ["foo True = select [True]"
            , ""
            ,"foo False = False"
            ]
          txtB' =
            [""
            ,"someOtherCode = ()"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" (T.unlines $ txtB ++ txtB')
      _ <- waitForDiagnostics
      InR action@CodeAction { _title = actionTitle } : _
                  <- sortOn (\(InR CodeAction{_title=x}) -> x) <$>
                     getCodeActions docB (R 0 0 0 50)
      liftIO $ actionTitle @?= "Define select :: [Bool] -> Bool"
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ contentAfterAction @?= T.unlines (txtB ++
        [ ""
        , "select :: [Bool] -> Bool"
        , "select = error \"not implemented\""
        ]
        ++ txtB')
  , testSession "define a hole" $ do
      let txtB =
            ["foo True = _select [True]"
            , ""
            ,"foo False = False"
            ]
          txtB' =
            [""
            ,"someOtherCode = ()"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" (T.unlines $ txtB ++ txtB')
      _ <- waitForDiagnostics
      InR action@CodeAction { _title = actionTitle } : _
                  <- sortOn (\(InR CodeAction{_title=x}) -> x) <$>
                     getCodeActions docB (R 0 0 0 50)
      liftIO $ actionTitle @?= "Define select :: [Bool] -> Bool"
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ contentAfterAction @?= T.unlines (
        ["foo True = select [True]"
        , ""
        ,"foo False = False"
        , ""
        , "select :: [Bool] -> Bool"
        , "select = error \"not implemented\""
        ]
        ++ txtB')
  ]

fixConstructorImportTests :: TestTree
fixConstructorImportTests = testGroup "fix import actions"
  [ testSession "fix constructor import" $ template
      (T.unlines
            [ "module ModuleA where"
            , "data A = Constructor"
            ])
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA(Constructor)"
            ])
      (Range (Position 1 10) (Position 1 11))
      "Fix import of A(Constructor)"
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA(A(Constructor))"
            ])
  ]
  where
    template contentA contentB range expectedAction expectedContentB = do
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      docB  <- createDoc "ModuleB.hs" "haskell" contentB
      _diags <- waitForDiagnostics
      InR action@CodeAction { _title = actionTitle } : _
                  <- sortOn (\(InR CodeAction{_title=x}) -> x) <$>
                     getCodeActions docB range
      liftIO $ expectedAction @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ expectedContentB @=? contentAfterAction

importRenameActionTests :: TestTree
importRenameActionTests = testGroup "import rename actions"
  [ testSession "Data.Mape -> Data.Map"   $ check "Map"
  , testSession "Data.Mape -> Data.Maybe" $ check "Maybe" ] where
  check modname = do
      let content = T.unlines
            [ "module Testing where"
            , "import Data.Mape"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 1 8) (Position 1 16))
      let [changeToMap] = [action | InR action@CodeAction{ _title = actionTitle } <- actionsOrCommands, ("Data." <> modname) `T.isInfixOf` actionTitle ]
      executeCodeAction changeToMap
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "import Data." <> modname
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction

fillTypedHoleTests :: TestTree
fillTypedHoleTests = let

  sourceCode :: T.Text -> T.Text -> T.Text -> T.Text
  sourceCode a b c = T.unlines
    [ "module Testing where"
      , ""
      , "globalConvert :: Int -> String"
      , "globalConvert = undefined"
      , ""
      , "globalInt :: Int"
      , "globalInt = 3"
      , ""
      , "bar :: Int -> Int -> String"
      , "bar n parameterInt = " <> a <> " (n + " <> b <> " + " <> c <> ")  where"
      , "  localConvert = (flip replicate) 'x'"

    ]

  check :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> TestTree
  check actionTitle
        oldA oldB oldC
        newA newB newC = testSession (T.unpack actionTitle) $ do
    let originalCode = sourceCode oldA oldB oldC
    let expectedCode = sourceCode newA newB newC
    doc <- createDoc "Testing.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 9 0) (Position 9 maxBound))
    chosenAction <- liftIO $ pickActionWithTitle actionTitle actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "fill typed holes"
  [ check "replace hole `_` with show"
          "_"    "n" "n"
          "show" "n" "n"

  , check "replace hole `_` with globalConvert"
          "_"             "n" "n"
          "globalConvert" "n" "n"

#if MIN_GHC_API_VERSION(8,6,0)
  , check "replace hole `_convertme` with localConvert"
          "_convertme"   "n" "n"
          "localConvert" "n" "n"
#endif

  , check "replace hole `_b` with globalInt"
          "_a" "_b"        "_c"
          "_a" "globalInt" "_c"

  , check "replace hole `_c` with globalInt"
          "_a" "_b"        "_c"
          "_a" "_b" "globalInt"

#if MIN_GHC_API_VERSION(8,6,0)
  , check "replace hole `_c` with parameterInt"
          "_a" "_b" "_c"
          "_a" "_b"  "parameterInt"
#endif
  ]

addSigActionTests :: TestTree
addSigActionTests = let
  header = "{-# OPTIONS_GHC -Wmissing-signatures -Wmissing-pattern-synonym-signatures #-}"
  moduleH = "{-# LANGUAGE PatternSynonyms #-}\nmodule Sigs where"
  before def     = T.unlines [header, moduleH,      def]
  after' def sig = T.unlines [header, moduleH, sig, def]

  def >:: sig = testSession (T.unpack def) $ do
    let originalCode = before def
    let expectedCode = after' def sig
    doc <- createDoc "Sigs.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 3 1) (Position 3 maxBound))
    chosenAction <- liftIO $ pickActionWithTitle ("add signature: " <> sig) actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "add signature"
    [ "abc = True"              >:: "abc :: Bool"
    , "foo a b = a + b"         >:: "foo :: Num a => a -> a -> a"
    , "bar a b = show $ a + b"  >:: "bar :: (Show a, Num a) => a -> a -> String"
    , "(!!!) a b = a > b"       >:: "(!!!) :: Ord a => a -> a -> Bool"
    , "a >>>> b = a + b"        >:: "(>>>>) :: Num a => a -> a -> a"
    , "a `haha` b = a b"        >:: "haha :: (t1 -> t2) -> t1 -> t2"
    , "pattern Some a = Just a" >:: "pattern Some :: a -> Maybe a"
    , testSession "-Werror with missing signatures" $ do
        doc <- createDoc "Sigs.hs" "haskell" $ T.unlines
          ["{-# OPTIONS_GHC -Werror -Wmissing-signatures #-}"
          , "module Sigs where"
          , "f = 42"
          ]
        expectDiagnostics [("Sigs.hs", [(DsError, (2, 0), "Top-level binding with no type signature")])]
        changeDoc doc [TextDocumentContentChangeEvent Nothing Nothing $ T.unlines
          ["{-# OPTIONS_GHC -Werror #-}"
          , "module Sigs where"
          , "f = 42"
          ]]
        expectDiagnostics [("Sigs.hs", [])]
    ]

addSigLensesTests :: TestTree
addSigLensesTests = let
  missing = "{-# OPTIONS_GHC -Wmissing-signatures -Wmissing-pattern-synonym-signatures -Wunused-matches #-}"
  notMissing = "{-# OPTIONS_GHC -Wunused-matches #-}"
  moduleH = "{-# LANGUAGE PatternSynonyms #-}\nmodule Sigs where"
  other = T.unlines ["f :: Integer -> Integer", "f x = 3"]
  before  withMissing def
    = T.unlines $ (if withMissing then (missing :) else (notMissing :)) [moduleH, def, other]
  after'  withMissing def sig
    = T.unlines $ (if withMissing then (missing :) else (notMissing :)) [moduleH, sig, def, other]

  sigSession withMissing def sig = testSession (T.unpack def) $ do
    let originalCode = before withMissing def
    let expectedCode = after' withMissing def sig
    doc <- createDoc "Sigs.hs" "haskell" originalCode
    [CodeLens {_command = Just c}] <- getCodeLenses doc
    executeCommand c
    modifiedCode <- skipManyTill anyMessage (getDocumentEdit doc)
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "add signature"
    [ testGroup title
      [ sigSession enableWarnings "abc = True"              "abc :: Bool"
      , sigSession enableWarnings "foo a b = a + b"         "foo :: Num a => a -> a -> a"
      , sigSession enableWarnings "bar a b = show $ a + b"  "bar :: (Show a, Num a) => a -> a -> String"
      , sigSession enableWarnings "(!!!) a b = a > b"       "(!!!) :: Ord a => a -> a -> Bool"
      , sigSession enableWarnings "a >>>> b = a + b"        "(>>>>) :: Num a => a -> a -> a"
      , sigSession enableWarnings "a `haha` b = a b"        "haha :: (t1 -> t2) -> t1 -> t2"
      , sigSession enableWarnings "pattern Some a = Just a" "pattern Some :: a -> Maybe a"
      ]
      | (title, enableWarnings) <-
        [("with warnings enabled", True)
        ,("with warnings disabled", False)
        ]
    ]

linkToLocation :: [LocationLink] -> [Location]
linkToLocation = map (\LocationLink{_targetUri,_targetRange} -> Location _targetUri _targetRange)

findDefinitionAndHoverTests :: TestTree
findDefinitionAndHoverTests = let

  tst :: (TextDocumentIdentifier -> Position -> Session a, a -> [Expect] -> Session ()) -> Position -> [Expect] -> String -> TestTree
  tst (get, check) pos targetRange title = testSession title $ do
    doc <- openTestDataDoc sourceFilePath
    found <- get doc pos
    check found targetRange

  checkDefs :: [Location] |? [LocationLink] -> [Expect] -> Session ()
  checkDefs (either id linkToLocation . toEither -> defs) expectations = traverse_ check expectations where

    check (ExpectRange expectedRange) = do
      assertNDefinitionsFound 1 defs
      assertRangeCorrect (head defs) expectedRange
    check ExpectNoDefinitions = do
      assertNDefinitionsFound 0 defs
    check ExpectExternFail = liftIO $ assertFailure "Expecting to fail to find in external file"
    check _ = pure () -- all other expectations not relevant to getDefinition

  assertNDefinitionsFound :: Int -> [a] -> Session ()
  assertNDefinitionsFound n defs = liftIO $ assertEqual "number of definitions" n (length defs)

  assertRangeCorrect Location{_range = foundRange} expectedRange =
    liftIO $ expectedRange @=? foundRange

  checkHover :: Maybe Hover -> [Expect] -> Session ()
  checkHover hover expectations = traverse_ check expectations where

    check expected =
      case hover of
        Nothing -> unless (expected == ExpectNoHover) $ liftIO $ assertFailure "no hover found"
        Just Hover{_contents = (HoverContents MarkupContent{_value = msg})
                  ,_range    = rangeInHover } ->
          case expected of
            ExpectRange  expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverRange expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverText snippets -> liftIO $ traverse_ (`assertFoundIn` msg) snippets
            ExpectNoHover -> liftIO $ assertFailure $ "Expected no hover but got " <> show hover
            _ -> pure () -- all other expectations not relevant to hover
        _ -> liftIO $ assertFailure $ "test not expecting this kind of hover info" <> show hover

  extractLineColFromHoverMsg :: T.Text -> [T.Text]
  extractLineColFromHoverMsg = T.splitOn ":" . head . T.splitOn "*" . last . T.splitOn (sourceFileName <> ":")

  checkHoverRange :: Range -> Maybe Range -> T.Text -> Session ()
  checkHoverRange expectedRange rangeInHover msg =
    let
      lineCol = extractLineColFromHoverMsg msg
      -- looks like hovers use 1-based numbering while definitions use 0-based
      -- turns out that they are stored 1-based in RealSrcLoc by GHC itself.
      adjust Position{_line = l, _character = c} =
        Position{_line = l + 1, _character = c + 1}
    in
    case map (read . T.unpack) lineCol of
      [l,c] -> liftIO $ (adjust $ _start expectedRange) @=? Position l c
      _     -> liftIO $ assertFailure $
        "expected: " <> show ("[...]" <> sourceFileName <> ":<LINE>:<COL>**[...]", Just expectedRange) <>
        "\n but got: " <> show (msg, rangeInHover)

  assertFoundIn :: T.Text -> T.Text -> Assertion
  assertFoundIn part whole = assertBool
    (T.unpack $ "failed to find: `" <> part <> "` in hover message:\n" <> whole)
    (part `T.isInfixOf` whole)

  sourceFilePath = T.unpack sourceFileName
  sourceFileName = "GotoHover.hs"

  mkFindTests tests = testGroup "get"
    [ testGroup "definition" $ mapMaybe fst tests
    , testGroup "hover"      $ mapMaybe snd tests ]

  test runDef runHover look expect title =
    ( runDef   $ tst def   look expect title
    , runHover $ tst hover look expect title ) where
      def   = (getDefinitions, checkDefs)
      hover = (getHover      , checkHover)
      --type_ = (getTypeDefinitions, checkTDefs) -- getTypeDefinitions always times out

  -- search locations            expectations on results
  fffL4  = _start fffR     ;  fffR = mkRange 4  4    4  7 ; fff  = [ExpectRange fffR]
  fffL8  = Position  8  4  ;
  fffL14 = Position 14  7  ;
  aaaL14 = Position 14 20  ;  aaa    = [mkR   7  0    7  3]
  dcL7   = Position  7 11  ;  tcDC   = [mkR   3 23    5 16]
  dcL12  = Position 12 11  ;
  xtcL5  = Position  5 11  ;  xtc    = [ExpectExternFail,   ExpectHoverText ["Int", "Defined in ‘GHC.Types’"]]
  tcL6   = Position  6 11  ;  tcData = [mkR   3  0    5 16, ExpectHoverText ["TypeConstructor", "GotoHover.hs:4:1"]]
  vvL16  = Position 16 12  ;  vv     = [mkR  16  4   16  6]
  opL16  = Position 16 15  ;  op     = [mkR  17  2   17  4]
  opL18  = Position 18 22  ;  opp    = [mkR  18 13   18 17]
  aL18   = Position 18 20  ;  apmp   = [mkR  18 10   18 11]
  b'L19  = Position 19 13  ;  bp     = [mkR  19  6   19  7]
  xvL20  = Position 20  8  ;  xvMsg  = [ExpectExternFail,   ExpectHoverText ["Data.Text.pack", ":: String -> Text"]]
  clL23  = Position 23 11  ;  cls    = [mkR  21  0   22 20, ExpectHoverText ["MyClass", "GotoHover.hs:22:1"]]
  clL25  = Position 25  9
  eclL15 = Position 15  8  ;  ecls   = [ExpectExternFail, ExpectHoverText ["Num", "Defined in ‘GHC.Num’"]]
  dnbL29 = Position 29 18  ;  dnb    = [ExpectHoverText [":: ()"],   mkR  29 12   29 21]
  dnbL30 = Position 30 23
  lcbL33 = Position 33 26  ;  lcb    = [ExpectHoverText [":: Char"], mkR  33 26   33 27]
  lclL33 = Position 33 22
  mclL36 = Position 36  1  ;  mcl    = [mkR  36  0   36 14]
  mclL37 = Position 37  1
  spaceL37 = Position 37  24 ; space = [ExpectNoDefinitions, ExpectHoverText [":: Char"]]
  docL41 = Position 41  1  ;  doc    = [ExpectHoverText ["Recognizable docs: kpqz"]]
                           ;  constr = [ExpectHoverText ["Monad m"]]
  eitL40 = Position 40 28  ;  kindE  = [ExpectHoverText [":: * -> * -> *\n"]]
  intL40 = Position 40 34  ;  kindI  = [ExpectHoverText [":: *\n"]]
  tvrL40 = Position 40 37  ;  kindV  = [ExpectHoverText [":: * -> *\n"]]
  intL41 = Position 41 20  ;  litI   = [ExpectHoverText ["7518"]]
  chrL36 = Position 37 24  ;  litC   = [ExpectHoverText ["'f'"]]
  txtL8  = Position  8 14  ;  litT   = [ExpectHoverText ["\"dfgy\""]]
  lstL43 = Position 43 12  ;  litL   = [ExpectHoverText ["[8391 :: Int, 6268]"]]
  outL45 = Position 45  3  ;  outSig = [ExpectHoverText ["outer", "Bool"], mkR 46 0 46 5]
  innL48 = Position 48  5  ;  innSig = [ExpectHoverText ["inner", "Char"], mkR 49 2 49 7]
  in
  mkFindTests
  --     def    hover  look   expect
  [ test yes    yes    fffL4  fff    "field in record definition"
  , test broken broken fffL8  fff    "field in record construction     #71"
  , test yes    yes    fffL14 fff    "field name used as accessor"          -- 120 in Calculate.hs
  , test yes    yes    aaaL14 aaa    "top-level name"                       -- 120
  , test yes    yes    dcL7   tcDC   "data constructor record         #247"
  , test yes    yes    dcL12  tcDC   "data constructor plain"               -- 121
  , test yes    yes    tcL6   tcData "type constructor                #248" -- 147
  , test broken yes    xtcL5  xtc    "type constructor external   #248,249"
  , test broken yes    xvL20  xvMsg  "value external package          #249" -- 120
  , test yes    yes    vvL16  vv     "plain parameter"                      -- 120
  , test yes    yes    aL18   apmp   "pattern match name"                   -- 120
  , test yes    yes    opL16  op     "top-level operator"                   -- 120, 123
  , test yes    yes    opL18  opp    "parameter operator"                   -- 120
  , test yes    yes    b'L19  bp     "name in backticks"                    -- 120
  , test yes    yes    clL23  cls    "class in instance declaration   #250"
  , test yes    yes    clL25  cls    "class in signature              #250" -- 147
  , test broken yes    eclL15 ecls   "external class in signature #249,250"
  , test yes    yes    dnbL29 dnb    "do-notation   bind"                   -- 137
  , test yes    yes    dnbL30 dnb    "do-notation lookup"
  , test yes    yes    lcbL33 lcb    "listcomp   bind"                      -- 137
  , test yes    yes    lclL33 lcb    "listcomp lookup"
  , test yes    yes    mclL36 mcl    "top-level fn 1st clause"
  , test yes    yes    mclL37 mcl    "top-level fn 2nd clause         #246"
  , test yes    yes    spaceL37 space "top-level fn on space #315"
  , test no     broken docL41 doc    "documentation                     #7"
  , test no     yes    eitL40 kindE  "kind of Either                  #273"
  , test no     yes    intL40 kindI  "kind of Int                     #273"
  , test no     broken tvrL40 kindV  "kind of (* -> *) type variable  #273"
  , test no     yes    intL41 litI   "literal Int  in hover info      #274"
  , test no     yes    chrL36 litC   "literal Char in hover info      #274"
  , test no     yes    txtL8  litT   "literal Text in hover info      #274"
  , test no     yes    lstL43 litL   "literal List in hover info      #274"
  , test no     yes    docL41 constr "type constraint in hover info   #283"
  , test broken broken outL45 outSig "top-level signature             #310"
  , test broken broken innL48 innSig "inner     signature             #310"
  ]
  where yes, broken :: (TestTree -> Maybe TestTree)
        yes    = Just -- test should run and pass
        broken = Just . (`xfail` "known broken")
        no = const Nothing -- don't run this test at all

pluginTests :: TestTree
pluginTests = testSessionWait "plugins" $ do
  let content =
        T.unlines
          [ "{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}"
          , "{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators #-}"
          , "module Testing where"
          , "import Data.Proxy"
          , "import GHC.TypeLits"
          -- This function fails without plugins being initialized.
          , "f :: forall n. KnownNat n => Proxy n -> Integer"
          , "f _ = natVal (Proxy :: Proxy n) + natVal (Proxy :: Proxy (n+2))"
          , "foo :: Int -> Int -> Int"
          , "foo a b = a + c"
          ]
  _ <- createDoc "Testing.hs" "haskell" content
  expectDiagnostics
    [ ( "Testing.hs",
        [(DsError, (8, 14), "Variable not in scope: c")]
      )
    ]

cppTests :: TestTree
cppTests =
  testGroup "cpp"
    [ testCase "cpp-error" $ do
        let content =
              T.unlines
                [ "{-# LANGUAGE CPP #-}",
                  "module Testing where",
                  "#ifdef FOO",
                  "foo = 42"
                ]
        -- The error locations differ depending on which C-preprocessor is used.
        -- Some give the column number and others don't (hence -1). Assert either
        -- of them.
        (run $ expectError content (2, -1))
          `catch` ( \e -> do
                      let _ = e :: HUnitFailure
                      run $ expectError content (2, 1)
                  )
    , testSessionWait "cpp-ghcide" $ do
        _ <- createDoc "A.hs" "haskell" $ T.unlines
          ["{-# LANGUAGE CPP #-}"
          ,"main ="
          ,"#ifdef __GHCIDE__"
          ,"  worked"
          ,"#else"
          ,"  failed"
          ,"#endif"
          ]
        expectDiagnostics [("A.hs", [(DsError, (3, 2), "Variable not in scope: worked")])]
    ]
  where
    expectError :: T.Text -> Cursor -> Session ()
    expectError content cursor = do
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs",
            [(DsError, cursor, "error: unterminated")]
          )
        ]
      expectNoMoreDiagnostics 0.5

preprocessorTests :: TestTree
preprocessorTests = testSessionWait "preprocessor" $ do
  let content =
        T.unlines
          [ "{-# OPTIONS_GHC -F -pgmF=ghcide-test-preprocessor #-}"
          , "module Testing where"
          , "y = x + z" -- plugin replaces x with y, making this have only one diagnostic
          ]
  _ <- createDoc "Testing.hs" "haskell" content
  expectDiagnostics
    [ ( "Testing.hs",
        [(DsError, (2, 8), "Variable not in scope: z")]
      )
    ]


safeTests :: TestTree
safeTests =
  testGroup
    "SafeHaskell"
    [ -- Test for https://github.com/digital-asset/ghcide/issues/424
      testSessionWait "load" $ do
        let sourceA =
              T.unlines
                ["{-# LANGUAGE Trustworthy #-}"
                ,"module A where"
                ,"import System.IO.Unsafe"
                ,"import System.IO"
                ,"trustWorthyId :: a -> a"
                ,"trustWorthyId i = unsafePerformIO $ do"
                ,"  putStrLn \"I'm safe\""
                ,"  return i"]
            sourceB =
              T.unlines
                ["{-# LANGUAGE Safe #-}"
                ,"module B where"
                ,"import A"
                ,"safeId :: a -> a"
                ,"safeId = trustWorthyId"
                ]

        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        expectNoMoreDiagnostics 1 ]

thTests :: TestTree
thTests =
  testGroup
    "TemplateHaskell"
    [ -- Test for https://github.com/digital-asset/ghcide/pull/212
      testSessionWait "load" $ do
        let sourceA =
              T.unlines
                [ "{-# LANGUAGE PackageImports #-}",
                  "{-# LANGUAGE TemplateHaskell #-}",
                  "module A where",
                  "import \"template-haskell\" Language.Haskell.TH",
                  "a :: Integer",
                  "a = $(litE $ IntegerL 3)"
                ]
            sourceB =
              T.unlines
                [ "{-# LANGUAGE PackageImports #-}",
                  "{-# LANGUAGE TemplateHaskell #-}",
                  "module B where",
                  "import A",
                  "import \"template-haskell\" Language.Haskell.TH",
                  "b :: Integer",
                  "b = $(litE $ IntegerL $ a) + n"
                ]
        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        expectDiagnostics [ ( "B.hs", [(DsError, (6, 29), "Variable not in scope: n")] ) ]
    , testSessionWait "newtype-closure" $ do
        let sourceA =
              T.unlines
                [ "{-# LANGUAGE DeriveDataTypeable #-}"
                  ,"{-# LANGUAGE TemplateHaskell #-}"
                  ,"module A (a) where"
                  ,"import Data.Data"
                  ,"import Language.Haskell.TH"
                  ,"newtype A = A () deriving (Data)"
                  ,"a :: ExpQ"
                  ,"a = [| 0 |]"]
        let sourceB =
              T.unlines
                [ "{-# LANGUAGE TemplateHaskell #-}"
                ,"module B where"
                ,"import A"
                ,"b :: Int"
                ,"b = $( a )" ]
        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        return ()
    ]

completionTests :: TestTree
completionTests
  = testGroup "completion"
    [ testSessionWait "variable" $ do
        let source = T.unlines ["module A where", "f = hea"]
        docId <- createDoc "A.hs" "haskell" source
        compls <- getCompletions docId (Position 1 7)
        liftIO $ map dropDocs compls @?=
          [complItem "head" (Just CiFunction) (Just "[a] -> a")]
        let [CompletionItem { _documentation = headDocs}] = compls
        checkDocText "head" headDocs [ "Defined in 'Prelude'"
#if MIN_GHC_API_VERSION(8,6,5)
                                     , "Extract the first element of a list"
#endif
                                     ]
    , testSessionWait "constructor" $ do
        let source = T.unlines ["module A where", "f = Tru"]
        docId <- createDoc "A.hs" "haskell" source
        compls <- getCompletions docId (Position 1 7)
        liftIO $ map dropDocs compls @?=
          [ complItem "True" (Just CiConstructor) (Just "Bool")
#if MIN_GHC_API_VERSION(8,6,0)
          , complItem "truncate" (Just CiFunction) (Just "(RealFrac a, Integral b) => a -> b")
#else
          , complItem "truncate" (Just CiFunction) (Just "RealFrac a => forall b. Integral b => a -> b")
#endif
          ]
    , testSessionWait "type" $ do
        let source = T.unlines ["{-# OPTIONS_GHC -Wall #-}", "module A () where", "f :: ()", "f = ()"]
        docId <- createDoc "A.hs" "haskell" source
        expectDiagnostics [ ("A.hs", [(DsWarning, (3,0), "not used")]) ]
        changeDoc docId [TextDocumentContentChangeEvent Nothing Nothing $ T.unlines ["{-# OPTIONS_GHC -Wall #-}", "module A () where", "f :: Bo", "f = True"]]
        compls <- getCompletions docId (Position 2 7)
        liftIO $ map dropDocs compls @?=
            [ complItem "Bounded" (Just CiClass) (Just "* -> Constraint")
            , complItem "Bool" (Just CiStruct) (Just "*") ]
        let [ CompletionItem { _documentation = boundedDocs},
              CompletionItem { _documentation = boolDocs } ] = compls
        checkDocText "Bounded" boundedDocs [ "Defined in 'Prelude'"
#if MIN_GHC_API_VERSION(8,6,5)
                                           , "name the upper and lower limits"
#endif
                                           ]
        checkDocText "Bool" boolDocs [ "Defined in 'Prelude'" ]
    , testSessionWait "qualified" $ do
        let source = T.unlines ["{-# OPTIONS_GHC -Wunused-binds #-}", "module A () where", "f = ()"]
        docId <- createDoc "A.hs" "haskell" source
        expectDiagnostics [ ("A.hs", [(DsWarning, (2, 0), "not used")]) ]
        changeDoc docId [TextDocumentContentChangeEvent Nothing Nothing $ T.unlines ["{-# OPTIONS_GHC -Wunused-binds #-}", "module A () where", "f = Prelude.hea"]]
        compls <- getCompletions docId (Position 2 15)
        liftIO $ map dropDocs compls @?=
          [complItem "head" (Just CiFunction) (Just "[a] -> a")]
        let [CompletionItem { _documentation = headDocs}] = compls
        checkDocText "head" headDocs [ "Defined in 'Prelude'"
#if MIN_GHC_API_VERSION(8,6,5)
                                     , "Extract the first element of a list"
#endif
                                     ]
    , testSessionWait "keyword" $ do
        let source = T.unlines ["module A where", "f = newty"]
        docId <- createDoc "A.hs" "haskell" source
        compls <- getCompletions docId (Position 1 9)
        liftIO $ compls @?= [keywordItem "newtype"]
    , testSessionWait "type context" $ do
        let source = T.unlines
                [ "{-# OPTIONS_GHC -Wunused-binds #-}"
                , "module A () where"
                , "f = f"
                ]
        docId <- createDoc "A.hs" "haskell" source
        expectDiagnostics [("A.hs", [(DsWarning, (2, 0), "not used")])]
        changeDoc docId
             [ TextDocumentContentChangeEvent Nothing Nothing $ T.unlines
                   [ "{-# OPTIONS_GHC -Wunused-binds #-}"
                   , "module A () where"
                   , "f = f"
                   , "g :: Intege"
                   ]
             ]
        -- At this point the module parses but does not typecheck.
        -- This should be sufficient to detect that we are in a
        -- type context and only show the completion to the type.
        compls <- getCompletions docId (Position 3 11)
        liftIO $ map dropDocs compls @?= [complItem "Integer"(Just CiStruct) (Just "*")]
    ]
  where
    dropDocs :: CompletionItem -> CompletionItem
    dropDocs ci = ci { _documentation = Nothing }
    complItem label kind ty = CompletionItem
      { _label = label
      , _kind = kind
      , _tags = Nothing
      , _detail = (":: " <>) <$> ty
      , _documentation = Nothing
      , _deprecated = Nothing
      , _preselect = Nothing
      , _sortText = Nothing
      , _filterText = Nothing
      , _insertText = Nothing
      , _insertTextFormat = Just PlainText
      , _textEdit = Nothing
      , _additionalTextEdits = Nothing
      , _commitCharacters = Nothing
      , _command = Nothing
      , _xdata = Nothing
      , _insertTextMode = Nothing
      }
    keywordItem label = CompletionItem
      { _label = label
      , _kind = Just CiKeyword
      , _tags = Nothing
      , _detail = Nothing
      , _documentation = Nothing
      , _deprecated = Nothing
      , _preselect = Nothing
      , _sortText = Nothing
      , _filterText = Nothing
      , _insertText = Nothing
      , _insertTextFormat = Nothing
      , _textEdit = Nothing
      , _additionalTextEdits = Nothing
      , _commitCharacters = Nothing
      , _command = Nothing
      , _xdata = Nothing
      , _insertTextMode = Nothing
      }
    getDocText (CompletionDocString s) = s
    getDocText (CompletionDocMarkup (MarkupContent _ s)) = s
    checkDocText thing Nothing _
      = liftIO $ assertFailure $ "docs for " ++ thing ++ " not found"
    checkDocText thing (Just doc) items
      = liftIO $ assertBool ("docs for " ++ thing ++ " contain the strings") $
          all (`T.isInfixOf` getDocText doc) items

outlineTests :: TestTree
outlineTests = testGroup
  "outline"
  [ testSessionWait "type class" $ do
    let source = T.unlines ["module A where", "class A a where a :: a -> Bool"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ moduleSymbol
          "A"
          (R 0 7 0 8)
          [ classSymbol "A a"
                        (R 1 0 1 30)
                        [docSymbol' "a" SkMethod (R 1 16 1 30) (R 1 16 1 17)]
          ]
      ]
  , testSessionWait "type class instance " $ do
    let source = T.unlines ["class A a where", "instance A () where"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ classSymbol "A a" (R 0 0 0 15) []
      , docSymbol "A ()" SkInterface (R 1 0 1 19)
      ]
  , testSessionWait "type family" $ do
    let source = T.unlines ["{-# language TypeFamilies #-}", "type family A"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbolD "A" "type family" SkClass (R 1 0 1 13)]
  , testSessionWait "type family instance " $ do
    let source = T.unlines
          [ "{-# language TypeFamilies #-}"
          , "type family A a"
          , "type instance A () = ()"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ docSymbolD "A a"   "type family" SkClass     (R 1 0 1 15)
      , docSymbol "A ()" SkInterface (R 2 0 2 23)
      ]
  , testSessionWait "data family" $ do
    let source = T.unlines ["{-# language TypeFamilies #-}", "data family A"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbolD "A" "data family" SkClass (R 1 0 1 11)]
  , testSessionWait "data family instance " $ do
    let source = T.unlines
          [ "{-# language TypeFamilies #-}"
          , "data family A a"
          , "data instance A () = A ()"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ docSymbolD "A a"   "data family" SkClass     (R 1 0 1 11)
      , docSymbol "A ()" SkInterface (R 2 0 2 25)
      ]
  , testSessionWait "constant" $ do
    let source = T.unlines ["a = ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbol "a" SkFunction (R 0 0 0 6)]
  , testSessionWait "pattern" $ do
    let source = T.unlines ["Just foo = Just 21"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbol "Just foo" SkFunction (R 0 0 0 18)]
  , testSessionWait "pattern with type signature" $ do
    let source = T.unlines ["{-# language ScopedTypeVariables #-}", "a :: () = ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbol "a :: ()" SkFunction (R 1 0 1 12)]
  , testSessionWait "function" $ do
    let source = T.unlines ["a x = ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbol "a" SkFunction (R 0 0 0 8)]
  , testSessionWait "type synonym" $ do
    let source = T.unlines ["type A = Bool"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbol' "A" SkTypeParameter (R 0 0 0 13) (R 0 5 0 6)]
  , testSessionWait "datatype" $ do
    let source = T.unlines ["data A = C"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ docSymbolWithChildren "A"
                              SkStruct
                              (R 0 0 0 10)
                              [docSymbol "C" SkConstructor (R 0 9 0 10)]
      ]
  , testSessionWait "import" $ do
    let source = T.unlines ["import Data.Maybe"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbolWithChildren "imports"
                             SkModule
                             (R 0 0 0 17)
                             [ docSymbol "import Data.Maybe" SkModule (R 0 0 0 17)
                             ]
      ]
  , testSessionWait "multiple import" $ do
    let source = T.unlines ["", "import Data.Maybe", "", "import Control.Exception", ""]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbolWithChildren "imports"
                             SkModule
                             (R 1 0 3 24)
                             [ docSymbol "import Data.Maybe" SkModule (R 1 0 1 17)
                             , docSymbol "import Control.Exception" SkModule (R 3 0 3 24)
                             ]
      ]
  , testSessionWait "foreign import" $ do
    let source = T.unlines
          [ "{-# language ForeignFunctionInterface #-}"
          , "foreign import ccall \"a\" a :: Int"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbolD "a" "import" SkObject (R 1 0 1 33)]
  , testSessionWait "foreign export" $ do
    let source = T.unlines
          [ "{-# language ForeignFunctionInterface #-}"
          , "foreign export ccall odd :: Int -> Bool"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbolD "odd" "export" SkObject (R 1 0 1 39)]
  ]
 where
  docSymbol name kind loc =
    DocumentSymbol name Nothing kind Nothing Nothing loc loc Nothing
  docSymbol' name kind loc selectionLoc =
    DocumentSymbol name Nothing kind Nothing Nothing loc selectionLoc Nothing
  docSymbolD name detail kind loc =
    DocumentSymbol name (Just detail) kind Nothing Nothing loc loc Nothing
  docSymbolWithChildren name kind loc cc =
    DocumentSymbol name Nothing kind Nothing Nothing loc loc (Just $ List cc)
  moduleSymbol name loc cc = DocumentSymbol name
                                            Nothing
                                            SkFile
                                            Nothing
                                            Nothing
                                            (R 0 0 maxBound 0)
                                            loc
                                            (Just $ List cc)
  classSymbol name loc cc = DocumentSymbol name
                                           (Just "class")
                                           SkClass
                                           Nothing
                                           Nothing
                                           loc
                                           loc
                                           (Just $ List cc)

pattern R :: UInt -> UInt -> UInt -> UInt -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

xfail :: TestTree -> String -> TestTree
xfail = flip expectFailBecause

data Expect
  = ExpectRange Range -- Both gotoDef and hover should report this range
--  | ExpectDefRange Range -- Only gotoDef should report this range
  | ExpectHoverRange Range -- Only hover should report this range
  | ExpectHoverText [T.Text] -- the hover message must contain these snippets
  | ExpectExternFail -- definition lookup in other file expected to fail
  | ExpectNoDefinitions
  | ExpectNoHover
--  | ExpectExtern -- TODO: as above, but expected to succeed: need some more info in here, once we have some working examples
  deriving Eq

mkR :: UInt -> UInt -> UInt -> UInt -> Expect
mkR startLine startColumn endLine endColumn = ExpectRange $ mkRange startLine startColumn endLine endColumn

haddockTests :: TestTree
haddockTests
  = testGroup "haddock"
      [ testCase "Num" $ checkHaddock
          (unlines
             [ "However, '(+)' and '(*)' are"
             , "customarily expected to define a ring and have the following properties:"
             , ""
             , "[__Associativity of (+)__]: @(x + y) + z@ = @x + (y + z)@"
             , "[__Commutativity of (+)__]: @x + y@ = @y + x@"
             , "[__@fromInteger 0@ is the additive identity__]: @x + fromInteger 0@ = @x@"
             ]
          )
          (unlines
             [ ""
             , ""
             , "However,  `(+)`  and  `(*)`  are"
             , "customarily expected to define a ring and have the following properties: "
             , "+ ****Associativity of (+)****: `(x + y) + z`  =  `x + (y + z)`"
             , "+ ****Commutativity of (+)****: `x + y`  =  `y + x`"
             , "+ ****`fromInteger 0`  is the additive identity****: `x + fromInteger 0`  =  `x`"
             ]
          )
      , testCase "unsafePerformIO" $ checkHaddock
          (unlines
             [ "may require"
             , "different precautions:"
             , ""
             , "  * Use @{\\-\\# NOINLINE foo \\#-\\}@ as a pragma on any function @foo@"
             , "        that calls 'unsafePerformIO'.  If the call is inlined,"
             , "        the I\\/O may be performed more than once."
             , ""
             , "  * Use the compiler flag @-fno-cse@ to prevent common sub-expression"
             , "        elimination being performed on the module."
             , ""
             ]
          )
          (unlines
             [ ""
             , ""
             , "may require"
             , "different precautions: "
             , "+ Use  `{-# NOINLINE foo #-}`  as a pragma on any function  `foo` "
             , "  that calls  `unsafePerformIO` .  If the call is inlined,"
             , "  the I/O may be performed more than once."
             , ""
             , "+ Use the compiler flag  `-fno-cse`  to prevent common sub-expression"
             , "  elimination being performed on the module."
             , ""
             ]
          )
      ]
  where
    checkHaddock s txt = spanDocToMarkdownForTest s @?= txt

cradleTests :: TestTree
cradleTests = testGroup "cradle"
    [testGroup "dependencies" [sessionDepsArePickedUp]
    ,testGroup "loading" [loadCradleOnlyonce]
    ]

loadCradleOnlyonce :: TestTree
loadCradleOnlyonce = testGroup "load cradle only once"
    [ testSession' "implicit" implicit
    , testSession' "direct"   direct
    ]
    where
        direct dir = do
            liftIO $ writeFileUTF8 (dir </> "hie.yaml")
                "cradle: {direct: {arguments: []}}"
            test dir
        implicit dir = test dir
        test _dir = do
            doc <- createDoc "B.hs" "haskell" "module B where\nimport Data.Foo"
            msgs <- someTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message STextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 1
            changeDoc doc [TextDocumentContentChangeEvent Nothing Nothing "module B where\nimport Data.Maybe"]
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message STextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0

            _ <- createDoc "A.hs" "haskell" "module A where\nimport Bar"
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message STextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0


cradleLoadedMessage :: Session FromServerMessage
cradleLoadedMessage = satisfy $ \case
        FromServerMess (SCustomMethod m) (NotMess _) -> m == cradleLoadedMethod
        _ -> False

cradleLoadedMethod :: T.Text
cradleLoadedMethod = "ghcide/cradle/loaded"

sessionDepsArePickedUp :: TestTree
sessionDepsArePickedUp = testSession'
  "session-deps-are-picked-up"
  $ \dir -> do
    liftIO $
      writeFileUTF8
        (dir </> "hie.yaml")
        "cradle: {direct: {arguments: []}}"
    -- Open without OverloadedStrings and expect an error.
    doc <- createDoc "Foo.hs" "haskell" fooContent
    expectDiagnostics
      [("Foo.hs", [(DsError, (3, 6), "Couldn't match expected type")])]
    -- Update hie.yaml to enable OverloadedStrings.
    liftIO $
      writeFileUTF8
        (dir </> "hie.yaml")
        "cradle: {direct: {arguments: [-XOverloadedStrings]}}"
    -- Send change event.
    let change =
          TextDocumentContentChangeEvent
            { _range = Just (Range (Position 4 0) (Position 4 0)),
              _rangeLength = Nothing,
              _text = "\n"
            }
    changeDoc doc [change]
    -- Now no errors.
    expectDiagnostics [("Foo.hs", [])]
  where
    fooContent =
      T.unlines
        [ "module Foo where",
          "import Data.Text",
          "foo :: Text",
          "foo = \"hello\""
        ]


----------------------------------------------------------------------
-- Utils


testSession :: String -> Session () -> TestTree
testSession name = testCase name . run

testSession' :: String -> (FilePath -> Session ()) -> TestTree
testSession' name = testCase name . run'

testSessionWait :: String -> Session () -> TestTree
testSessionWait name = testSession name .
      -- Check that any diagnostics produced were already consumed by the test case.
      --
      -- If in future we add test cases where we don't care about checking the diagnostics,
      -- this could move elsewhere.
      --
      -- Experimentally, 0.5s seems to be long enough to wait for any final diagnostics to appear.
      ( >> expectNoMoreDiagnostics 0.5)

pickActionWithTitle :: T.Text -> [Command |? CodeAction] -> IO CodeAction
pickActionWithTitle title actions = do
  assertBool ("Found no matching actions: " <> show titles) (not $ null matches)
  return $ head matches
  where
    titles =
        [ actionTitle
        | InR CodeAction { _title = actionTitle } <- actions
        ]
    matches =
        [ action
        | InR action@CodeAction { _title = actionTitle } <- actions
        , title == actionTitle
        ]

mkRange :: UInt -> UInt -> UInt -> UInt -> Range
mkRange a b c d = Range (Position a b) (Position c d)

run :: Session a -> IO a
run s = withTempDir $ \dir -> runInDir dir s

run' :: (FilePath -> Session a) -> IO a
run' s = withTempDir $ \dir -> runInDir dir (s dir)

runInDir :: FilePath -> Session a -> IO a
runInDir dir s = do
  ghcideExe <- locateGhcideExecutable

  -- Temporarily hack around https://github.com/mpickering/hie-bios/pull/56
  -- since the package import test creates "Data/List.hs", which otherwise has no physical home
  createDirectoryIfMissing True $ dir ++ "/Data"

  let cmd = unwords [ghcideExe, "--lsp", "--test", "--cwd", dir]
  -- HIE calls getXgdDirectory which assumes that HOME is set.
  -- Only sets HOME if it wasn't already set.
  setEnv "HOME" "/homeless-shelter" False
  let lspTestCaps = fullCaps { _window = Just $ WindowClientCapabilities (Just True) Nothing Nothing }
  runSessionWithConfig conf cmd lspTestCaps dir s
  where
    conf = defaultConfig
      -- If you uncomment this you can see all logging
      -- which can be quite useful for debugging.
      -- { logStdErr = True, logColor = False }
      -- If you really want to, you can also see all messages
      -- { logMessages = True, logColor = False }

openTestDataDoc :: FilePath -> Session TextDocumentIdentifier
openTestDataDoc path = do
  source <- liftIO $ readFileUtf8 $ "test/data" </> path
  createDoc path "haskell" source

findCodeActions :: TextDocumentIdentifier -> Maybe Range -> [T.Text] -> Session [CodeAction]
findCodeActions doc range expectedTitles = do
  actions <- maybe (getAllCodeActions doc) (getCodeActions doc) range
  let matches = sequence
        [ listToMaybe
          [ action
          | InR action@CodeAction { _title = actionTitle } <- actions
          , actionTitle == expectedTitle ]
        | expectedTitle <- expectedTitles]
  let msg = show
            [ actionTitle
            | InR CodeAction { _title = actionTitle } <- actions
            ]
            ++ " is not a superset of "
            ++ show expectedTitles
  liftIO $ case matches of
    Nothing -> assertFailure msg
    Just _ -> pure ()
  return (fromJust matches)

findCodeAction :: TextDocumentIdentifier -> Maybe Range -> T.Text -> Session CodeAction
findCodeAction doc range t = head <$> findCodeActions doc range [t]

unitTests :: TestTree
unitTests = do
  testGroup "Unit"
     [ testCase "empty file path does NOT work with the empty String literal" $
         uriToFilePath' (fromNormalizedUri $ filePathToUri' "") @?= Just "."
     , testCase "empty file path works using toNormalizedFilePath'" $
         uriToFilePath' (fromNormalizedUri $ filePathToUri' (toNormalizedFilePath' "")) @?= Just ""
     , testCase "empty path URI" $ do
         Just URI{..} <- pure $ parseURI (T.unpack $ getUri $ fromNormalizedUri emptyPathUri)
         uriScheme @?= "file:"
         uriPath @?= ""
     , testCase "from empty path URI" $ do
         let uri = Uri "file://"
         uriToFilePath' uri @?= Just ""
     ]

positionMappingTests :: TestTree
positionMappingTests =
    testGroup "position mapping"
        [ testGroup "toCurrent"
              [ testCase "before" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 0) @?= Just (Position 0 0)
              , testCase "after, same line, same length" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 3) @?= Just (Position 0 3)
              , testCase "after, same line, increased length" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 0 3) @?= Just (Position 0 4)
              , testCase "after, same line, decreased length" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "a"
                    (Position 0 3) @?= Just (Position 0 2)
              , testCase "after, next line, no newline" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 1 3) @?= Just (Position 1 3)
              , testCase "after, next line, newline" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\ndef"
                    (Position 1 0) @?= Just (Position 2 0)
              , testCase "after, same line, newline" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd"
                    (Position 0 4) @?= Just (Position 1 2)
              , testCase "after, same line, newline + newline at end" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd\n"
                    (Position 0 4) @?= Just (Position 2 1)
              , testCase "after, same line, newline + newline at end" $
                toCurrent
                    (Range (Position 0 1) (Position 0 1))
                    "abc"
                    (Position 0 1) @?= Just (Position 0 4)
              ]
        , testGroup "fromCurrent"
              [ testCase "before" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 0) @?= Just (Position 0 0)
              , testCase "after, same line, same length" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 3) @?= Just (Position 0 3)
              , testCase "after, same line, increased length" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 0 4) @?= Just (Position 0 3)
              , testCase "after, same line, decreased length" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "a"
                    (Position 0 2) @?= Just (Position 0 3)
              , testCase "after, next line, no newline" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 1 3) @?= Just (Position 1 3)
              , testCase "after, next line, newline" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\ndef"
                    (Position 2 0) @?= Just (Position 1 0)
              , testCase "after, same line, newline" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd"
                    (Position 1 2) @?= Just (Position 0 4)
              , testCase "after, same line, newline + newline at end" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd\n"
                    (Position 2 1) @?= Just (Position 0 4)
              , testCase "after, same line, newline + newline at end" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 1))
                    "abc"
                    (Position 0 4) @?= Just (Position 0 1)
              ]
        , adjustOption (\(QuickCheckTests i) -> QuickCheckTests (max 1000 i)) $ testGroup "properties"
              [ testProperty "fromCurrent r t <=< toCurrent r t" $ do
                -- Note that it is important to use suchThatMap on all values at once
                -- instead of only using it on the position. Otherwise you can get
                -- into situations where there is no position that can be mapped back
                -- for the edit which will result in QuickCheck looping forever.
                let gen = do
                        rope <- genRope
                        range <- genRange rope
                        PrintableText replacement <- arbitrary
                        oldPos <- genPosition rope
                        pure (range, replacement, oldPos)
                forAll
                    (suchThatMap gen
                        (\(range, replacement, oldPos) -> (range, replacement, oldPos,) <$> toCurrent range replacement oldPos)) $
                    \(range, replacement, oldPos, newPos) ->
                    fromCurrent range replacement newPos === Just oldPos
              , testProperty "toCurrent r t <=< fromCurrent r t" $ do
                let gen = do
                        rope <- genRope
                        range <- genRange rope
                        PrintableText replacement <- arbitrary
                        let newRope = applyChange rope (TextDocumentContentChangeEvent (Just range) Nothing replacement)
                        newPos <- genPosition newRope
                        pure (range, replacement, newPos)
                forAll
                    (suchThatMap gen
                        (\(range, replacement, newPos) -> (range, replacement, newPos,) <$> fromCurrent range replacement newPos)) $
                    \(range, replacement, newPos, oldPos) ->
                    toCurrent range replacement oldPos === Just newPos
              ]
        ]

newtype PrintableText = PrintableText { getPrintableText :: T.Text }
    deriving Show

instance Arbitrary PrintableText where
    arbitrary = PrintableText . T.pack . getPrintableString <$> arbitrary


genRope :: Gen Rope
genRope = Rope.fromText . getPrintableText <$> arbitrary

genPosition :: Rope -> Gen Position
genPosition r = do
    let rows = Rope.rows r
    row <- choose (0, max 0 $ rows - 1) `suchThat` inBounds @UInt
    let columns = Rope.columns (nthLine row r)
    column <- choose (0, max 0 $ columns - 1) `suchThat` inBounds @UInt
    pure $ Position (fromIntegral row) (fromIntegral column)

genRange :: Rope -> Gen Range
genRange r = do
    let rows = Rope.rows r
    startPos@(Position startLine startColumn) <- genPosition r
    let maxLineDiff = max 0 $ rows - 1 - fromIntegral startLine
    endLine <- choose (fromIntegral startLine, fromIntegral startLine + maxLineDiff) `suchThat` inBounds @UInt
    let columns = Rope.columns (nthLine (fromIntegral endLine) r)
    endColumn <-
        if fromIntegral startLine == endLine
            then choose (fromIntegral startColumn, columns)
            else choose (0, max 0 $ columns - 1)
        `suchThat` inBounds @UInt
    pure $ Range startPos (Position (fromIntegral endLine) (fromIntegral endColumn))

inBounds :: forall b a . (Integral a, Integral b, Bounded b) => a -> Bool
inBounds a = let i = toInteger a in i <= toInteger (maxBound @b) && i >= toInteger (minBound @b)

-- | Get the ith line of a rope, starting from 0. Trailing newline not included.
nthLine :: Int -> Rope -> Rope
nthLine i r
    | i < 0 = error $ "Negative line number: " <> show i
    | i == 0 && Rope.rows r == 0 = r
    | i >= Rope.rows r = error $ "Row number out of bounds: " <> show i <> "/" <> show (Rope.rows r)
    | otherwise = Rope.takeWhile (/= '\n') $ fst $ Rope.splitAtLine 1 $ snd $ Rope.splitAtLine (i - 1) r

getWatchedFilesSubscriptionsUntil :: forall m. SServerMethod m -> Session [DidChangeWatchedFilesRegistrationOptions]
getWatchedFilesSubscriptionsUntil end = do
      msgs <- manyTill (Just <$> message SClientRegisterCapability <|> Nothing <$ anyMessage) (message end)
      return
            [ args
            | Just RequestMessage{_params = RegistrationParams (List regs)} <- msgs
            , SomeRegistration (Registration _id SWorkspaceDidChangeWatchedFiles args) <- regs
            ]
