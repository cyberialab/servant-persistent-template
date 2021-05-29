{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module WordsCount.API where
import WordsCount.Domain ( SortBy, WordCount, WordsToCount, textToCount, countWords, sortArray )
import Servant.Multipart
    ( MultipartForm,
      FileData(fdPayload),
      Input(iValue),
      MultipartData(inputs, files),
      Mem )
import Servant
import Servant.Auth.Server (AuthResult(..))
import Control.Monad.Logger ( logDebugNS )
import Control.Monad.Reader (MonadIO, liftIO)
import Config
import qualified Data.ByteString.Lazy as LBS
import Codec.Binary.UTF8.String (decode)
import Control.Monad
import Authentication.Domain.Models (AuthenticatedUser, Authorized)
import Control.Exception.Safe (throwIO)

type ProtectedWordsCountAPI =
       "words" :> "count" :> MultipartForm Mem (MultipartData Mem) :> QueryParam "sortBy" SortBy :> Post '[JSON] [WordCount] 

type PublicWordsCountAPI =
       "public" :> "words" :> "count" :> ReqBody '[JSON] WordsToCount :> QueryParam "sortBy" SortBy :> Post '[JSON] [WordCount]

type WordsCountAPI =
  (Authorized :> ProtectedWordsCountAPI) :<|> PublicWordsCountAPI

countWordsFromFileAPI :: (MonadIO m) => (AuthResult AuthenticatedUser) -> MultipartData Mem -> Maybe SortBy -> AppT m [WordCount]
countWordsFromFileAPI (Authenticated _) multipartData sortBy_ = do
  logDebugNS "WordsCount" "countWordsFromFileAPI"
  let res1 = forM (inputs multipartData) $ \input -> countWords (show (iValue input))
  
      res2 = forM (files multipartData) $ \file -> do
                 let content = fdPayload file
                 countWords (decode $ LBS.unpack content)

  return $ sortArray (concat (res1 ++ res2)) sortBy_
countWordsFromFileAPI _ _ _ = liftIO $ throwIO err401

countWordsFromTextAPI :: (MonadIO m) => WordsToCount -> Maybe SortBy -> AppT m [WordCount]
countWordsFromTextAPI text sortBy_ = do
  logDebugNS "WordsCount" "countWordsFromFileAPI"
  let countedWords = countWords $ textToCount text
  return $ sortArray countedWords sortBy_

countWordsAPI :: (MonadIO m) => ServerT WordsCountAPI (AppT m)
countWordsAPI = countWordsFromFileAPI :<|> countWordsFromTextAPI
