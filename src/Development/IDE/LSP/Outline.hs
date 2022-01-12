{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
#include "ghc-api-version.h"

module Development.IDE.LSP.Outline
  ( moduleOutline
  )
where

import           Language.LSP.Server (LspM)
import           Language.LSP.Types             (DocumentSymbol (..),
                                                 DocumentSymbolParams (DocumentSymbolParams, _textDocument),
                                                 List (..), ResponseError,
                                                 SymbolInformation,
                                                 SymbolKind (..),
                                                 TextDocumentIdentifier (TextDocumentIdentifier),
                                                 type (|?) (InL), uriToFilePath)
import           Control.Monad.IO.Class
import           Data.Functor
import           Data.Generics
import           Data.Maybe
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as T
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error      ( srcSpanToRange )
import           Development.IDE.Types.Location
import           Outputable                     ( Outputable
                                                , ppr
                                                , showSDocUnsafe
                                                )

moduleOutline
  :: IdeState -> DocumentSymbolParams -> LspM c (Either ResponseError (List DocumentSymbol |? List SymbolInformation))
moduleOutline ideState DocumentSymbolParams { _textDocument = TextDocumentIdentifier uri }
  = liftIO $ case uriToFilePath uri of
    Just (toNormalizedFilePath' -> fp) -> do
      mb_decls <- runAction ideState $ use GetParsedModule fp
      pure $ Right $ case mb_decls of
        Nothing -> InL (List [])
        Just ParsedModule { pm_parsed_source = L _ltop HsModule { hsmodName, hsmodDecls, hsmodImports } }
          -> let
               declSymbols  = mapMaybe documentSymbolForDecl hsmodDecls
               moduleSymbol = hsmodName <&> \(L l m) ->
                 (defDocumentSymbol l :: DocumentSymbol)
                   { _name  = pprText m
                   , _kind  = SkFile
                   , _range = Range (Position 0 0) (Position maxBound 0) -- _ltop is 0 0 0 0
                   }
               importSymbols = maybe [] pure $
                  documentSymbolForImportSummary
                    (mapMaybe documentSymbolForImport hsmodImports)
               allSymbols    = case moduleSymbol of
                 Nothing -> importSymbols <> declSymbols
                 Just x ->
                   [ x { _children = Just (List (importSymbols <> declSymbols))
                       }
                   ]
             in
               InL (List allSymbols)


    Nothing -> pure $ Right $ InL (List [])

documentSymbolForDecl :: Located (HsDecl GhcPs) -> Maybe DocumentSymbol
documentSymbolForDecl (L l (TyClD FamDecl { tcdFam = FamilyDecl { fdLName = L _ n, fdInfo, fdTyVars } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name   = showRdrName n
                  <> (case pprText fdTyVars of
                       "" -> ""
                       t  -> " " <> t
                     )
    , _detail = Just $ pprText fdInfo
    , _kind   = SkClass
    }
documentSymbolForDecl (L l (TyClD ClassDecl { tcdLName = L _ name, tcdSigs, tcdTyVars }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name     = showRdrName name
                    <> (case pprText tcdTyVars of
                         "" -> ""
                         t  -> " " <> t
                       )
    , _kind     = SkClass
    , _detail   = Just "class"
    , _children =
      Just $ List
        [ (defDocumentSymbol l :: DocumentSymbol)
            { _name           = showRdrName n
            , _kind           = SkMethod
            , _selectionRange = srcSpanToRange l'
            }
        | L l  (ClassOpSig False names _) <- tcdSigs
        , L l' n                            <- names
        ]
    }
documentSymbolForDecl (L l (TyClD DataDecl { tcdLName = L _ name, tcdDataDefn = HsDataDefn { dd_cons } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name     = showRdrName name
    , _kind     = SkStruct
    , _children =
      Just $ List
        [ (defDocumentSymbol l :: DocumentSymbol)
            { _name           = showRdrName n
            , _kind           = SkConstructor
            , _selectionRange = srcSpanToRange l'
            }
        | L l  x <- dd_cons
        , L l' n <- getConNames x
        ]
    }
documentSymbolForDecl (L l (TyClD SynDecl { tcdLName = L l' n })) = Just
  (defDocumentSymbol l :: DocumentSymbol) { _name           = showRdrName n
                                          , _kind           = SkTypeParameter
                                          , _selectionRange = srcSpanToRange l'
                                          }
documentSymbolForDecl (L l (InstD ClsInstD { cid_inst = ClsInstDecl { cid_poly_ty } }))
  | isDamlInternalClInst cid_poly_ty = Nothing
  | otherwise = Just (defDocumentSymbol l :: DocumentSymbol) { _name = pprText cid_poly_ty
                                                 , _kind = SkInterface
                                                 }
documentSymbolForDecl (L l (InstD DataFamInstD { dfid_inst = DataFamInstDecl HsIB { hsib_body = FamEqn { feqn_tycon, feqn_pats } } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name = showRdrName (unLoc feqn_tycon) <> " " <> T.unwords
                (map pprText feqn_pats)
    , _kind = SkInterface
    }
documentSymbolForDecl (L l (InstD TyFamInstD { tfid_inst = TyFamInstDecl HsIB { hsib_body = FamEqn { feqn_tycon, feqn_pats } } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name = showRdrName (unLoc feqn_tycon) <> " " <> T.unwords
                (map pprText feqn_pats)
    , _kind = SkInterface
    }
documentSymbolForDecl (L l (DerivD DerivDecl { deriv_type })) =
  gfindtype deriv_type <&> \(L (_ :: SrcSpan) name) ->
    (defDocumentSymbol l :: DocumentSymbol) { _name = pprText @(HsType GhcPs)
                                              name
                                            , _kind = SkInterface
                                            }
documentSymbolForDecl (L l (ValD FunBind{fun_id = L _ name}))
  | "_choice_" `T.isPrefixOf` showRdrName name = Nothing
  | otherwise = Just
    (defDocumentSymbol l :: DocumentSymbol)
      { _name   = showRdrName name
      , _kind   = SkFunction
      }
documentSymbolForDecl (L l (ValD PatBind{pat_lhs})) = Just
    (defDocumentSymbol l :: DocumentSymbol)
      { _name   = pprText pat_lhs
      , _kind   = SkFunction
      }

documentSymbolForDecl (L l (ForD x)) = Just
  (defDocumentSymbol l :: DocumentSymbol)
    { _name   = case x of
                  ForeignImport{} -> name
                  ForeignExport{} -> name
#if MIN_GHC_API_VERSION(8,6,0)
                  XForeignDecl{}  -> "?"
#endif
    , _kind   = SkObject
    , _detail = case x of
                  ForeignImport{} -> Just "import"
                  ForeignExport{} -> Just "export"
#if MIN_GHC_API_VERSION(8,6,0)
                  XForeignDecl{}  -> Nothing
#endif
    }
  where name = showRdrName $ unLoc $ fd_name x

documentSymbolForDecl _ = Nothing

-- | Wrap the Document imports into a hierarchical outline for
-- a better overview of symbols in scope.
-- If there are no imports, then no hierarchy will be created.
documentSymbolForImportSummary :: [DocumentSymbol] -> Maybe DocumentSymbol
documentSymbolForImportSummary [] = Nothing
documentSymbolForImportSummary importSymbols =
    let
      -- safe because if we have no ranges then we don't take this branch
      mergeRanges xs = Range (minimum $ map _start xs) (maximum $ map _end xs)
      importRange = mergeRanges $ map (_range :: DocumentSymbol -> Range) importSymbols
    in
      Just (defDocumentSymbol' importRange)
          { _name = "imports"
          , _kind = SkModule
          , _children = Just (List importSymbols)
          }

documentSymbolForImport :: Located (ImportDecl GhcPs) -> Maybe DocumentSymbol
documentSymbolForImport (L l ImportDecl { ideclName, ideclQualified, ideclImplicit })
  | ideclImplicit = Nothing
  | otherwise = Just
  (defDocumentSymbol l :: DocumentSymbol)
    { _name   = "import " <> pprText ideclName
    , _kind   = SkModule
#if MIN_GHC_API_VERSION(8,10,0)
    , _detail = case ideclQualified of { NotQualified -> Nothing; _ -> Just "qualified" }
#else
    , _detail = if ideclQualified then Just "qualified" else Nothing
#endif
    }
#if MIN_GHC_API_VERSION(8,6,0)
documentSymbolForImport (L _ XImportDecl {}) = Nothing
#endif

defDocumentSymbol :: SrcSpan -> DocumentSymbol
defDocumentSymbol l = defDocumentSymbol' (srcSpanToRange l)

defDocumentSymbol' :: Range -> DocumentSymbol
defDocumentSymbol' l = DocumentSymbol { .. } where
  _detail         = Nothing
  _deprecated     = Nothing
  _name           = ""
  _kind           = SkUnknown 0
  _range          = l
  _selectionRange = l
  _children       = Nothing
  _tags           = Nothing

showRdrName :: RdrName -> Text
showRdrName = pprText

pprText :: Outputable a => a -> Text
pprText = pack . showSDocUnsafe . ppr

-- Daml specific filters.
isDamlInternalClInst:: LHsSigType GhcPs -> Bool
isDamlInternalClInst cid_poly_ty =
  "DA.Internal.Record" `T.isPrefixOf` pprText cid_poly_ty ||
  "DA.Internal.Desugar" `T.isPrefixOf` pprText cid_poly_ty
