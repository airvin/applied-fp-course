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

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Data.Bifunctor                     (Bifunctor (..))

import           Database.SQLite.Simple             (Connection, Query (Query))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level04.Types                      (Comment, CommentText,
                                                    Error(..), Topic, getTopic, fromDBComment)
import           Level04.DB.Types                   (DBComment(..))


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
      Sql.runDBAction $ 
        getDBComments >>= (\dbComments -> traverse fromDBComment dbComments >>= (\comment -> pure comment)) 
        >>= (\eitherSQLError -> bimap DBError id eitherSQLError)
      
    
  -- fromDBComment :: DBComment -> Either Error Comment

  -- query :: FromRow r => Connection -> Query -> q -> IO [r]

  -- From the getDBComments, bind, etc. we have Either Error [Comment]

  -- runDBAction :: IO a -> IO (DatabaseResponse a)
  -- type DatabaseResponse a = Either SQLiteResponse a

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  in
    error "addCommentToTopic not implemented (use Sql.runDBAction to catch exceptions)"

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics =
  let
    sql = "SELECT DISTINCT topic FROM comments"
  in
    error "getTopics not implemented (use Sql.runDBAction to catch exceptions)"

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
  in
    error "deleteTopic not implemented (use Sql.runDBAction to catch exceptions)"
