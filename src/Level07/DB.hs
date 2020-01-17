{-# LANGUAGE OverloadedStrings #-}
module Level07.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level07.AppM                      (App, Env (envDB), ask, AppM(..))

import           Level07.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError),
                                                     FirstAppDB (FirstAppDB, dbConn),
                                                     Topic, fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: App Connection
-- getDBConn = asks (\env -> dbConn $ envDB env)
getDBConn = asks (dbConn . envDB)

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> App b
runDB fn conn2ioA = getDBConn >>= (\conn -> let ioEitherA = Sql.runDBAction $ conn2ioA conn in 
  AppM (\_ -> either (Left . DBError) fn <$> ioEitherA))

-- runDB fn conn2ioA = getDBConn >>= (\conn -> let ioEitherA = Sql.runDBAction $ conn2ioA conn in 
--   AppM (\_ -> either (Left . DBError) fn <$> ioEitherA))

-- liftIO :: IO a -> AppM e a
-- liftEither :: Either e a -> AppM e a

-- runDBAction :: IO a -> IO (DatabaseResponse a)
-- DatabaseResponse a = Either SQLiteResponse a

-- either :: (e -> c) -> (a -> c) -> either e a -> c
-- where c is Either DBError b

-- runDB
--   :: (a -> Either Error b)
--   -> IO a
--   -> App b
-- runDB f a = do
--   r <- liftIO $ first DBError <$> Sql.runDBAction a
--   liftEither $ f =<< r


getComments
  :: Topic
  -> App [Comment]
getComments t = do
    let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    runDB (traverse fromDBComment) (\conn -> Sql.query conn q (Sql.Only . getTopic $ t))
              
-- query :: Connection -> Query -> q -> IO [r]
-- Level 7 runDB :: (a -> Either Error b) -> (Connection -> IO a) -> App b
-- Level 6 runDB :: (a -> Either Error b) ->  IO a ->                App b

addCommentToTopic
  :: Topic
  -> CommentText
  -> App ()
addCommentToTopic t c = do 
  nowish <- liftIO getCurrentTime
  let q =
        "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  runDB Right $ (\conn -> Sql.execute conn q (getTopic t, getCommentText c, nowish))

getTopics
  :: App [Topic]
getTopics =
  let q = "SELECT DISTINCT topic FROM comments"
  in
    runDB (traverse ( mkTopic . Sql.fromOnly )) $ (\conn -> Sql.query_  conn q)

deleteTopic
  :: Topic
  -> App ()
deleteTopic t = 
  let q = "DELETE FROM comments WHERE topic = ?"
  in
    runDB Right $ (\conn -> Sql.execute conn q (Sql.Only . getTopic $ t))

-- Go on to 'src/Level07/Core.hs' next.
