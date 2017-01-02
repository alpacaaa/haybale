{-# LANGUAGE OverloadedStrings #-}

module Lib (
  makeRequest
, fakeRequestUploadFile
, startServer
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import qualified Network.Http.Client as Http
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SL

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams

import OpenSSL (withOpenSSL)

import qualified Snap.Core as Snap
import Snap.Http.Server (quickHttpServe)

import qualified Web.JWT as JWT
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Map.Strict as Map

dropboxtoken = fmap (head . lines) (readFile "dropbox-token")



-- makeRequest :: IO ()
makeRequest uploadStream = withOpenSSL $ do
  putStrLn "uploading"

  code <- dropboxtoken
  ctx <- Http.baselineContextSSL
  c <- Http.openConnectionSSL ctx "content.dropboxapi.com" 443

  q <- Http.buildRequest $ do
    Http.http Http.POST "/2/files/upload"
    Http.setHeader "Content-Type" "application/octet-stream"
    Http.setHeader "Dropbox-API-Arg" "{ \"path\": \"/bingo.txt\", \"mode\": \"overwrite\"}"
    Http.setHeader "Authorization" $ SL.pack ("Bearer " ++ code)

  Http.sendRequest c q uploadStream

  Http.receiveResponse c Http.debugHandler

  Http.closeConnection c

  putStrLn "yo"


fakeRequestUploadFile =
  makeRequest $ Http.fileBody "haskell-haybale.cabal"

{--
  SNAP
  https://hackage.haskell.org/package/snap-core-1.0.1.0/docs/Snap-Core.html#v:transformRequestBody

  http-streams
  https://hackage.haskell.org/package/http-streams-0.8.4.0/docs/Network-Http-Client.html#v:inputStreamBody
--}

startServer :: IO ()
startServer = quickHttpServe site

site :: Snap.Snap ()
site =
  Snap.route [
    ("/", home),
    ("upload", upload),
    ("jwt", testJwt)
    ]



upload = do
  Snap.writeBS "upload route"
  Snap.runRequestBody $ makeRequest . Http.inputStreamBody


home =
  Snap.writeBS "Welcome"


testJwt = do
  let key = JWT.secret "donuts"
  let claim = JWT.def {
    JWT.unregisteredClaims = Map.fromList [
      ("something", "value")
    ]
  }
  let token = JWT.encodeSigned JWT.HS256 key claim

  let wrongsecret = JWT.secret "wrong"
  let result = JWT.decode token
  let verify = JWT.verify wrongsecret =<< result

  -- Snap.writeBS $ SL.pack $ show result

  Snap.writeBS $ SL.pack $ show verify

  -- Snap.writeBS $ encodeUtf8 token
  -- Snap.writeBS $ SL.pack (show result)
