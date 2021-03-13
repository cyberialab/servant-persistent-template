{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module WordsCount.API where
import WordsCount.Domain ( SortBy, WordCount, countWords, sortArray )

import Servant.Multipart
import Servant ( JSON, QueryParam, type (:>), Post, Server )

import qualified Data.ByteString.Lazy as LBS
import Codec.Binary.UTF8.String (decode)

import Control.Monad

type WordsCountAPI =
  "words" :> "count" :> MultipartForm Mem (MultipartData Mem) :> QueryParam "sortBy" SortBy :> Post '[JSON] [WordCount]

countWordsAPI :: Server WordsCountAPI
countWordsAPI multipartData sortBy_ = do
  let res1 = forM (inputs multipartData) $ \input -> countWords (show (iValue input))
  
      res2 = forM (files multipartData) $ \file -> do
                 let content = fdPayload file
                 countWords (decode $ LBS.unpack content)
                 
  return $ sortArray (concat (res1 ++ res2)) sortBy_