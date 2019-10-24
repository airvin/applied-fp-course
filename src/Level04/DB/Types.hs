{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level04.DB.Types where

import           Data.Text                      (Text)
import           Data.Time                      (UTCTime)

import           Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import           Control.Monad                  (liftM4)
-- To try to avoid leaking various types and expected functionality around the
-- application, we create a stand alone type that will represent the data we
-- store in the database. In this instance, it is the raw types that make up a
-- comment.

-- Complete in the DBComment type below so it is a record type that matches the
-- Comment type, but without the newtype wrappers for each value. To get started,
-- just copy the new definition for the `Comment` type from Level04.Types.
data DBComment = DBComment
  { _commentId    :: Int
  , _commentTopic :: Text
  , _commentBody  :: Text
  , _commentTime  :: UTCTime
  }
  -- NB: Haskell does not allow duplicate field names for records so the field
  -- names for this type will have to be slightly different

-- This Typeclass comes from the `sqlite-simple` package and describes how to
-- decode a single row from the database into a single representation of our
-- type. This technique of translating a result row to a type will differ
-- between different packages/databases.
instance FromRow DBComment where
  -- fromRow = (\i t c ti -> DBComment i t c ti)
  --           -- field :: FromField a => RowParser a
  --           <$> field
  --           <*> field
  --           <*> field
  --           <*> field
  fromRow = liftM4 DBComment field field field field

-- Now move to ``src/Level04/Types.hs``
