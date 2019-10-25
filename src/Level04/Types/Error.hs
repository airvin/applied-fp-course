{-# LANGUAGE OverloadedStrings #-}
module Level04.Types.Error
  ( Error(..)
  , nonEmptyText
  ) where

import       Data.Text                          (Text)
import       Database.SQLite.SimpleErrors.Types (SQLiteResponse)

data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  | EmptyId
  | DBError SQLiteResponse
  -- Add another constructor for our DB error types.
  deriving (Eq, Show)

nonEmptyText
  :: (Text -> a)
  -> Error
  -> Text
  -> Either Error a
nonEmptyText _ e "" = Left e
nonEmptyText c _ tx = Right (c tx)
