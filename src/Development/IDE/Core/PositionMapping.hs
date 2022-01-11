-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
module Development.IDE.Core.PositionMapping
  ( PositionMapping(..)
  , toCurrentRange
  , fromCurrentRange
  , applyChange
  , idMapping
  -- toCurrent and fromCurrent are mainly exposed for testing
  , toCurrent
  , fromCurrent
  ) where

import Control.Monad
import qualified Data.Text as T
import Language.LSP.Types

data PositionMapping = PositionMapping
  { toCurrentPosition :: !(Position -> Maybe Position)
  , fromCurrentPosition :: !(Position -> Maybe Position)
  }

toCurrentRange :: PositionMapping -> Range -> Maybe Range
toCurrentRange mapping (Range a b) =
    Range <$> toCurrentPosition mapping a <*> toCurrentPosition mapping b

fromCurrentRange :: PositionMapping -> Range -> Maybe Range
fromCurrentRange mapping (Range a b) =
    Range <$> fromCurrentPosition mapping a <*> fromCurrentPosition mapping b

idMapping :: PositionMapping
idMapping = PositionMapping Just Just

applyChange :: PositionMapping -> TextDocumentContentChangeEvent -> PositionMapping
applyChange posMapping (TextDocumentContentChangeEvent (Just r) _ t) = PositionMapping
    { toCurrentPosition = toCurrent r t <=< toCurrentPosition posMapping
    , fromCurrentPosition = fromCurrentPosition posMapping <=< fromCurrent r t
    }
applyChange posMapping _ = posMapping

toCurrent :: Range -> T.Text -> Position -> Maybe Position
toCurrent (Range (Position startLine startColumn) (Position endLine endColumn)) t (Position line column)
    | line < startLine || line == startLine && column < startColumn =
      -- Position is before the change and thereby unchanged.
      Just $ Position line column
    | line > endLine || line == endLine && column >= endColumn =
      -- Position is after the change so increase line and column number
      -- as necessary.
      Just $ newLine `seq` newColumn `seq` Position newLine newColumn
    | otherwise = Nothing
    -- Position is in the region that was changed.
    where
        lineDiff = linesNew - linesOld
        linesNew = T.count "\n" t
        linesOld = fromIntegral endLine - fromIntegral startLine
        newEndColumn :: UInt
        newEndColumn
          | linesNew == 0 = fromIntegral $ fromIntegral startColumn + T.length t
          | otherwise = fromIntegral $ T.length $ T.takeWhileEnd (/= '\n') t
        newColumn :: UInt
        newColumn
          | line == endLine = fromIntegral $ (fromIntegral column + newEndColumn) - fromIntegral endColumn
          | otherwise = column
        newLine :: UInt
        newLine = fromIntegral $ fromIntegral line + lineDiff

fromCurrent :: Range -> T.Text -> Position -> Maybe Position
fromCurrent (Range (Position startLine startColumn) (Position endLine endColumn)) t (Position line column)
    | line < startLine || line == startLine && column < startColumn =
      -- Position is before the change and thereby unchanged
      Just $ Position line column
    | line > newEndLine || line == newEndLine && column >= newEndColumn =
      -- Position is after the change so increase line and column number
      -- as necessary.
      Just $ newLine `seq` newColumn `seq` Position newLine newColumn
    | otherwise = Nothing
    -- Position is in the region that was changed.
    where
        lineDiff = linesNew - linesOld
        linesNew = T.count "\n" t
        linesOld = fromIntegral endLine - fromIntegral startLine
        newEndLine :: UInt
        newEndLine = fromIntegral $ fromIntegral endLine + lineDiff
        newEndColumn :: UInt
        newEndColumn
          | linesNew == 0 = fromIntegral $ fromIntegral startColumn + T.length t
          | otherwise = fromIntegral $ T.length $ T.takeWhileEnd (/= '\n') t
        newColumn :: UInt
        newColumn
          | line == newEndLine = fromIntegral $ (fromIntegral column + fromIntegral endColumn) - newEndColumn
          | otherwise = column
        newLine :: UInt
        newLine = fromIntegral $ fromIntegral line - lineDiff
