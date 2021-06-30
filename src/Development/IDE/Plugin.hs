{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Development.IDE.Plugin
    ( Plugin(..)
    , PluginCommand(..)
    , pluginHandler
    , pluginNotificationHandler
    , commandsHandler
    , commandIds
    , allPluginHandlers

    , codeActionPlugin
    , codeActionPluginWithRules
    ) where

import Control.Applicative
import Data.Aeson
import Data.Default
import           Data.Dependent.Map              (DMap)
import qualified Data.Dependent.Map              as DMap
import           Data.Dependent.Sum
import           Data.GADT.Compare
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Development.Shake (Rules)
import Development.IDE.LSP.Server

import           Language.LSP.Types
import Development.IDE.Core.Rules
import qualified Language.LSP.Server as LSP


data Plugin c = Plugin
    {pluginRules :: Rules ()
    ,pluginCommands :: [PluginCommand c]
    ,pluginHandlers :: PluginHandlers c
    ,pluginNotificationHandlers :: PluginNotificationHandlers c
    }

pluginHandler :: PluginMethod m => SMethod m -> (IdeState -> MessageParams m -> LSP.LspM c (Either ResponseError (ResponseResult m))) -> PluginHandlers c
pluginHandler m f = PluginHandlers (DMap.singleton (PluginHandlerMethod m) (PluginHandler f))

pluginNotificationHandler :: forall (m :: Method 'FromClient 'Notification) c. SMethod m -> (IdeState -> MessageParams m -> LSP.LspM c ()) -> PluginNotificationHandlers c
pluginNotificationHandler m f = PluginNotificationHandlers (DMap.singleton m (PluginNotificationHandler f))

class PluginMethod (m :: Method 'FromClient 'Request) where
    combineResponses :: SMethod m -> ResponseResult m -> ResponseResult m -> ResponseResult m

instance PluginMethod 'TextDocumentCodeAction where
    combineResponses _ = (<>)

instance PluginMethod 'TextDocumentCompletion where
    combineResponses _ a b = case (a, b) of
        (InL a, InL b) -> InL (a <> b)
        (InL a, InR (CompletionList complete b)) -> InR (CompletionList complete (a <> b))
        (InR (CompletionList complete a), InL b) -> InR (CompletionList complete (a <> b))
        (InR (CompletionList completeA a), InR (CompletionList completeB b)) -> InR (CompletionList (completeA && completeB) (a <> b))

instance PluginMethod 'TextDocumentCodeLens where
    combineResponses _ = (<>)

instance PluginMethod 'CustomMethod where
    -- later handlers override earlier handlers
    combineResponses _ _ b = b

newtype PluginHandler c (m :: Method 'FromClient 'Request)
  = PluginHandler (IdeState -> MessageParams m -> LSP.LspM c (Either ResponseError (ResponseResult m)))

newtype PluginHandlers c = PluginHandlers { getPluginHandlers :: DMap PluginHandlerMethod (PluginHandler c) }

data PluginHandlerMethod (m :: Method 'FromClient 'Request) = PluginMethod m => PluginHandlerMethod (SMethod m)

instance GEq PluginHandlerMethod where
  geq (PluginHandlerMethod a) (PluginHandlerMethod b) = geq a b
instance GCompare PluginHandlerMethod where
  gcompare (PluginHandlerMethod a) (PluginHandlerMethod b) = gcompare a b

instance Semigroup (PluginHandlers c) where
    PluginHandlers a <> PluginHandlers b =
        PluginHandlers (DMap.unionWithKey go a b)
      where
        go (PluginHandlerMethod m) (PluginHandler f) (PluginHandler g) = PluginHandler $ \ide params ->
          liftA2 (combineResponses m) <$> f ide params <*> g ide params

instance Monoid (PluginHandlers c) where
    mempty = PluginHandlers mempty

newtype PluginNotificationHandler c (m :: Method 'FromClient 'Notification)
  = PluginNotificationHandler (IdeState -> MessageParams m -> LSP.LspM c ())

newtype PluginNotificationHandlers c = PluginNotificationHandlers { getPluginNotificationHandlers :: DMap SMethod (PluginNotificationHandler c) }

instance Semigroup (PluginNotificationHandlers c) where
    PluginNotificationHandlers a <> PluginNotificationHandlers b =
        PluginNotificationHandlers (DMap.unionWithKey go a b)
      where
        go _ (PluginNotificationHandler f) (PluginNotificationHandler g) = PluginNotificationHandler $ \ide params ->
          (<>) <$> f ide params <*> g ide params

instance Monoid (PluginNotificationHandlers c) where
    mempty = PluginNotificationHandlers mempty


allPluginHandlers :: Plugin c -> LSP.Handlers (ServerM c)
allPluginHandlers p@Plugin{..} =
    mconcat [ notificationHandler m h | m :=> (PluginNotificationHandler h) <- DMap.toList (getPluginNotificationHandlers pluginNotificationHandlers) ] <>
    mconcat [ requestHandler m h | PluginHandlerMethod m :=> (PluginHandler h) <- DMap.toList (getPluginHandlers pluginHandlers) ] <>
    commandsHandler p

instance Default (Plugin c) where
    def = Plugin mempty mempty mempty mempty

instance Semigroup (Plugin c) where
    Plugin a1 b1 c1 d1 <> Plugin a2 b2 c2 d2 = Plugin (a1<>a2) (b1<>b2) (c1<>c2) (d1<>d2)

instance Monoid (Plugin c) where
    mempty = def

data PluginCommand c = forall a . FromJSON a => PluginCommand
  { commandId :: T.Text
  , commandHandler :: IdeState -> a -> LSP.LspM c (Either ResponseError Value)
  }

commandsHandler :: Plugin c -> LSP.Handlers (ServerM c)
commandsHandler Plugin{pluginCommands} = requestHandler SWorkspaceExecuteCommand $ \ideState (ExecuteCommandParams _ cmdId args) ->
   let cmdParams :: Value
       cmdParams = case args of
          Just (List (x:_)) -> x
          _                   -> Null
   in case Map.lookup cmdId cmdMap of
        Nothing -> pure $ Left $ ResponseError InvalidParams "Invalid command identifier" Nothing
        Just PluginCommand{..} -> case fromJSON cmdParams of
            Success a -> commandHandler ideState a
            Error err -> pure $ Left $ ResponseError InvalidParams
              ("Invalid command arguments for " <> commandId <> ": " <> T.pack err)
              Nothing
  where cmdMap = Map.fromList [(commandId cmd, cmd) | cmd <- pluginCommands]

commandIds :: Plugin c  -> [T.Text]
commandIds = map commandId . pluginCommands

codeActionPlugin :: (IdeState -> TextDocumentIdentifier -> Range -> CodeActionContext -> LSP.LspM c (Either ResponseError (List (Command |? CodeAction)))) -> Plugin c
codeActionPlugin = codeActionPluginWithRules mempty

codeActionPluginWithRules :: Rules () -> (IdeState -> TextDocumentIdentifier -> Range -> CodeActionContext -> LSP.LspM c (Either ResponseError (List (Command |? CodeAction)))) -> Plugin c
codeActionPluginWithRules rr f = Plugin
  { pluginRules = rr
  , pluginCommands = []
  , pluginHandlers =
    pluginHandler STextDocumentCodeAction $ \ide CodeActionParams{..} ->
      f ide _textDocument _range _context
  , pluginNotificationHandlers = mempty
  }
