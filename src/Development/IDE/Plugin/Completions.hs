{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Development.IDE.Plugin.Completions(plugin) where

import Control.Applicative
import Data.Maybe
import qualified Language.LSP.Server as LSP
import Language.LSP.Types
import qualified Language.LSP.VFS as VFS
import Language.LSP.Types.Capabilities
import Development.Shake.Classes
import Development.Shake
import GHC.Generics

import Development.IDE.Plugin
import Development.IDE.Core.Service
import Development.IDE.Plugin.Completions.Logic
import Development.IDE.Types.Location
import Development.IDE.Core.PositionMapping
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Shake
import Development.IDE.GHC.Util
import Development.IDE.Import.DependencyInformation


plugin :: Plugin c
plugin = Plugin produceCompletions [] (pluginHandler STextDocumentCompletion getCompletionsLSP) mempty

produceCompletions :: Rules ()
produceCompletions =
    define $ \ProduceCompletions file -> do
        deps <- maybe (TransitiveDependencies [] [] []) fst <$> useWithStale GetDependencies file
        parsedDeps <- mapMaybe (fmap fst) <$> usesWithStale GetParsedModule (transitiveModuleDeps deps)
        tm <- fmap fst <$> useWithStale TypeCheck file
        packageState <- fmap (hscEnv . fst) <$> useWithStale GhcSession file
        case (tm, packageState) of
            (Just tm', Just packageState') -> do
                cdata <- liftIO $ cacheDataProducer packageState'
                                                    (tmrModule tm') parsedDeps
                return ([], Just cdata)
            _ -> return ([], Nothing)


-- | Produce completions info for a file
type instance RuleResult ProduceCompletions = CachedCompletions

data ProduceCompletions = ProduceCompletions
    deriving (Eq, Show, Typeable, Generic)
instance Hashable ProduceCompletions
instance NFData   ProduceCompletions
instance Binary   ProduceCompletions


-- | Generate code actions.
getCompletionsLSP
    :: IdeState
    -> CompletionParams
    -> LSP.LspM c (Either ResponseError (List CompletionItem |? CompletionList))
getCompletionsLSP ide
  CompletionParams{_textDocument=TextDocumentIdentifier uri
                  ,_position=position
                  ,_context=completionContext} = do
    contents <- LSP.getVirtualFile $ toNormalizedUri uri
    liftIO $ fmap Right $ case (contents, uriToFilePath' uri) of
      (Just cnts, Just path) -> do
        let npath = toNormalizedFilePath' path
        (ideOpts, compls) <- runAction ide $ do
            opts <- getIdeOptions
            compls <- useWithStale ProduceCompletions npath
            pm <- useWithStale GetParsedModule npath
            pure (opts, liftA2 (,) compls pm)
        case compls of
          Just ((cci', _), (pm, mapping)) -> do
            let !position' = fromCurrentPosition mapping position
            pfix <- maybe (return Nothing) (flip VFS.getCompletionPrefix cnts) position'
            case (pfix, completionContext) of
              (Just (VFS.PosPrefixInfo _ "" _ _), Just CompletionContext { _triggerCharacter = Just "."})
                -> return (InL $ List [])
              (Just pfix', _) -> do
                let fakeClientCapabilities = ClientCapabilities Nothing Nothing Nothing Nothing
                InL . List <$> getCompletions ideOpts cci' pm pfix' fakeClientCapabilities (WithSnippets True)
              _ -> return (InL $ List [])
          _ -> return (InL $ List [])
      _ -> return (InL $ List [])
