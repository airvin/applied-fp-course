{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types            (ContentType (..), Error (..), RqType (..),
                                           mkCommentText, mkTopic, getCommentText, getTopic,
                                           renderContentType)

import           Control.Applicative      (liftA2)
-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status contentType lbs = 
  responseLBS
  status
  [("Content-Type", renderContentType contentType)]
  lbs

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 contentType lbs = mkResponse status200 contentType lbs 

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 contentType lbs = mkResponse status404 contentType lbs 

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 contentType lbs = mkResponse status400 contentType lbs 

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
-- mkAddRequest text lbs = let topic = (mkTopic text) in 
--   case topic of 
--     Left err -> Left err
--     Right t -> let comment = (mkCommentText $ lazyByteStringToStrictText lbs) in case comment of
--       Left err -> Left err
--       Right c -> Right $ AddRq t c
-- mkAddRequest text lbs = AddRq <$> mkTopic text <*> (mkCommentText $ lazyByteStringToStrictText lbs)
mkAddRequest text lbs = liftA2 AddRq (mkTopic text) (mkCommentText $ lazyByteStringToStrictText lbs)
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest text = ViewRq <$> (mkTopic text) 

mkListRequest
  :: Either Error RqType
mkListRequest = Right ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse err = case err of
  EmptyTopic -> resp400 PlainText "Specify a topic"
  EmptyComment -> resp400 PlainText "Comment can't be empty"
  NotFound -> resp404 PlainText "Not found"

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest req  = let path = (pathInfo req)
                     method = (requestMethod req)
                     body = (strictRequestBody req) in
                     case (method, path) of 
                      ("GET", ["list"]) -> pure mkListRequest
                      ("GET", [topic, "view"]) -> pure (mkViewRequest topic)
                      ("POST", [topic, "add"]) -> mkAddRequest topic <$> body
                      _ -> pure $ Left NotFound

-- pathInfo :: Request -> [Text]
-- requestMethod :: Request -> Method 
-- responseLBS,
-- strictRequestBody :: Request -> IO ByteString

  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest rqType = case rqType of 
  ListRq -> Right (resp200 PlainText "Here is a list of topics")
  ViewRq _ -> Right (resp200 PlainText "Here is a topic") 
  AddRq _ _ -> Right (resp200 PlainText "You submitted a request")
  

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app
  :: Application
app req cb = 
  mkRequest req >>= (\eitherRqType -> cb $ handleErrorResponse (eitherRqType >>= handleRequest))
  
  where 
    handleErrorResponse :: Either Error Response -> Response
    handleErrorResponse = either mkErrorResponse id
    
runApp :: IO ()
runApp = run 3000 app
