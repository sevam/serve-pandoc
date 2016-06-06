{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Main where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Resource
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson.Compat
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as B          
import           Data.ByteString.Lazy         (ByteString, unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Time.Calendar
import           GHC.Generics
import           Lucid
import           Network.HTTP.Media ((//), (/:))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Parse
import           Servant
import           Servant.Server.Internal
import           System.Directory
import           Text.Blaze
import qualified Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8
import           Control.Monad.Trans.Resource (runResourceT,withInternalState)

import           Pandoc

--------------------------------------------------------------------------------

data Mem
data Tmp

class KnownBackend b where
  type Storage b :: *
  withBackend :: Proxy b -> (BackEnd (Storage b) -> IO r) -> IO r

instance KnownBackend Mem where
  type Storage Mem = ByteString
  withBackend Proxy f = f lbsBackEnd

instance KnownBackend Tmp where
  type Storage Tmp = FilePath
  withBackend Proxy f =
    runResourceT . withInternalState $ \s -> f (tempFileBackEnd s)

data Files b

type MultiPartData b = ([Param], [File (Storage b)])
type MultiPartDataT b = ((MultiPartData b -> IO (MultiPartData b)) -> IO (MultiPartData b))

type FilesMem = Files Mem
type FilesTmp = Files Tmp

instance (KnownBackend b, HasServer sublayout config) => HasServer (Files b :> sublayout) config where
  type ServerT (Files b :> sublayout) m =
    MultiPartDataT b -> ServerT sublayout m

  route Proxy config subserver = route (Proxy :: Proxy sublayout) config (addBodyCheck subserver bodyCheck)
    where
      bodyCheck = withRequest $ \ request ->
             return $ \f -> withBackend (Proxy :: Proxy b) $ \pb -> parseRequestBody pb request >>= f

--------------------------------------------------------------------------------

type API = "convert" :> FilesMem :> Post '[JSON] ()
      :<|> "static"  :> Raw


handleFiles :: MultiPartDataT Mem -> IO ()
handleFiles multipart = void $ multipart $ \(params, files) -> do
  putStrLn "----------------------------------"
  putStrLn "Files:\n"
  mapM_ ppFile files
  mapM_ writeFileTmp files
  mapM_ convert files
  -- TODO: build return link
  putStrLn "\nParams:"
  print params
  putStrLn "----------------------------------"
  
  return (params, files)
  where
    
    ppFile :: File ByteString -> IO ()
    ppFile fl@(name', fileinfo) = do
      putStrLn $ "Input name:   " <> B.unpack name'
      putStrLn $ "File name:    " <> B.unpack (fileName fileinfo)
      putStrLn $ "Content type: " <> B.unpack (fileContentType fileinfo)
      putStrLn $ "Load:         \n" ++ BLC.unpack (fileContent fileinfo)

    writeFileTmp :: File ByteString -> IO ()
    writeFileTmp fl@(name', fileinfo) = do
      let filepath = "static/out/"++ (B.unpack $ fileName fileinfo) 
      BLC.writeFile filepath (fileContent fileinfo)
      
    convert :: File ByteString -> IO ByteString
    convert fl@(name', fileinfo) = do
      pdfEE <- convertHtmlToPdf (BLC.unpack $ fileContent fileinfo) ""
      case pdfEE of
        Left  err  ->
          do
            putStrLn $ ("PDF creation failed:\n" ++ (BLC.unpack err))
            return $ ""
        Right pdfE ->
          case pdfE of
            Left  err ->
              do
                putStrLn $ ("PDF creation failed:\n" ++ (BLC.unpack err))
                return $ ""
            Right pdf -> return $ pdf  
    

convertHandler :: MultiPartDataT Mem -> ExceptT ServantErr IO ()
convertHandler multipart = do
  liftIO $ handleFiles multipart    
  
server :: Server API
server =    convertHandler
       :<|> serveDirectory "/out"
     
-- Proxy for connecting servant
-- with Wai
api :: Proxy API
api  = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve api server

main :: IO ()
main = run 8081 app
