{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Level04.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad                                (fmap)
import           Control.Monad.IO.Class                       (liftIO)

import           Data.Text                                    (Text)
import qualified Data.Text                                    as Text

import           Data.Time                                    (getCurrentTime)

import           Data.Bifunctor                               (Bifunctor (..))

import           Database.SQLite.Simple                       (Connection, Query (Query))
import qualified Database.SQLite.Simple                       as Sql
import            Database.SQLite.Simple.Time.Implementation  (utcTimeToBuilder)

import qualified Database.SQLite.SimpleErrors                 as Sql
import           Database.SQLite.SimpleErrors.Types           (SQLiteResponse)

import           Level04.Types                                (Comment(..), CommentText,
                                                              Error(..), Topic, getTopic, mkTopic, fromDBComment, getCommentText)
import           Level04.DB.Types                             (DBComment(..))


-- ------------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple & sqlite-simple-errors handy! |
-- ------------------------------------------------------------------------------|

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
--
-- To help with that, we create a new data type that can hold our `Connection`
-- for us, and allows it to be expanded later if we need to
data FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB firstApp = Sql.close $ dbConn firstApp

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
-- initDB fp = let ioConn = (Sql.open fp) in 
--     Sql.runDBAction 
--       FirstAppDB <$> (const ioConn (ioConn >>= (\conn -> Sql.execute_ conn createTableQ)) ) 

initDB fp = Sql.runDBAction $ Sql.open fp >>= (\conn -> const (pure $ FirstAppDB conn) (Sql.execute_ conn createTableQ))

  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DBComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DBComment to a Comment, we need to use ``fromDBComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments firstAppDB topic =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?" 
    getDBComments :: IO [DBComment]
    getDBComments = Sql.query (dbConn firstAppDB) sql (Sql.Only $ getTopic topic)
    in 
      (traverse fromDBComment =<<) <$> (first DBError <$> Sql.runDBAction getDBComments)
      -- Sql.runDBAction getDBComments
      --   >>= (\dbResponseDBcomments -> pure $ first DBError dbResponseDBcomments >>= (\dbComments -> traverse fromDBComment dbComments))

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic firstAppDB topic comment =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    -- currentTime = liftIO getCurrentTime
    addComments = Sql.execute (dbConn firstAppDB) sql (getTopic topic, getCommentText comment, 123 :: Int)
  in
    first DBError <$> Sql.runDBAction addComments


-- runDBAction :: IO a -> IO (DBResp a)
-- execute :: ToRow q => Connection -> Query -> [q] -> IO ()

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics firstAppDB =
  let
    sql = "SELECT DISTINCT topic FROM comments"
    getDBComments :: IO [DBComment]
    getDBComments = Sql.query_ (dbConn firstAppDB) sql
  in
    (traverse fromDBComment =<<) 
    <$> (first DBError <$> Sql.runDBAction getDBComments) 
        >>= (\eitherErrorComments -> pure $ eitherErrorComments 
          >>= (\comments -> pure $ commentTopic <$> comments))


deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic firstAppDB topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    deleteTopics = Sql.execute (dbConn firstAppDB) sql (Sql.Only $ getTopic topic)
  in
    first DBError <$> Sql.runDBAction deleteTopics
