{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.Bifunctor             (Bifunctor (..))

import           Data.ByteString            (ByteString)

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             (first, second)
import           Data.Monoid                (Last (Last))

import           Control.Monad.IO.Class     (liftIO)
import           Control.Exception          (try)

import qualified Data.Attoparsec.ByteString as AB
import           Waargonaut.Attoparsec      (pureDecodeAttoparsecByteString)

import           Data.ByteString.Char8      as BC

import           Waargonaut                 (Json)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed, ConversionFailure))

import           Level06.AppM               (AppM (AppM))
import           Level06.Types              (ConfigError (BadConfFile, ConfigFileReadError),
                                             PartialConf (PartialConf),
                                             partialConfDecoder)
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> runAppM $ readConfFile "badFileName.no"
-- Left (<YourErrorConstructorHere> "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> runAppM $ readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile fp = AppM $ bimap ConfigFileReadError BC.pack <$> (try $ Prelude.readFile fp)

  -- readFile :: FilePath -> IO String
  -- try :: Exception e => IO a -> IO (Either e a)

  -- Reading a file may throw an exception for any number of
  -- reasons. Use the 'try' function from 'Control.Exception' to catch
  -- the exception and turn it into an error value that is thrown as
  -- part of our 'AppM' transformer.
  --
  -- No exceptions from reading the file should escape this function.
  --


-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile =
  error "parseJSONConfigFile not implemented"
  
-- Go to 'src/Level06/Conf.hs' next.
