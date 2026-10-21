-- | Reading and writing the text files Stronghead keeps and loads: always
-- UTF-8, whatever the system's code page (Windows' is not UTF-8, and titles
-- carry "…" and "µs").
module Stronghead.TextFile
  ( readTextFile
  , writeTextFile
  , saveTextFile
  ) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeDirectory, (<.>))

import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE

import Stronghead.Format (tshow)

-- | A text file read as UTF-8. Bytes that are not UTF-8 are replaced rather
-- than failing the read.
readTextFile :: FilePath -> IO Text
readTextFile path = TE.decodeUtf8With TE.lenientDecode <$> B.readFile path

-- | Write a text file as UTF-8, replacing it only once the new one is
-- whole.
writeTextFile :: FilePath -> Text -> IO ()
writeTextFile path txt = do
  let tmp = path <.> "tmp"
  B.writeFile tmp (TE.encodeUtf8 txt)
  renameFile tmp path

-- | 'writeTextFile', creating the directory first, with a failure said as
-- "Could not save @what@: …".
saveTextFile :: Text -> FilePath -> Text -> IO (Either Text ())
saveTextFile what path txt =
  first (\(e :: IOException) -> "Could not save " <> what <> ": " <> tshow e)
    <$> try do
      createDirectoryIfMissing True (takeDirectory path)
      writeTextFile path txt
