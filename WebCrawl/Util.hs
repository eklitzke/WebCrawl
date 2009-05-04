module WebCrawl.Util where

import WebCrawl.Globals
import Control.Concurrent.Chan
import Network.URI

getTodoURI :: Int -> IO (Maybe URI)
getTodoURI n = do
  empty <- isEmptyChan todoChan
  if empty
       then return Nothing
       else do (num, uri) <- readChan todoChan
               if num < n
                  then return $ Just uri
                  else do unGetChan todoChan (num, uri)
                          return Nothing

-- todo: make lazy
readChanWhile :: (a -> Bool) -> Chan a -> IO [a]
readChanWhile p c = do
  cs <- getChanContents c
  return $ takeWhile p cs

getTodoUris :: Int -> IO [URI]
getTodoUris n = do
  todo <- getTodoURI n
  case todo of
    Nothing -> return []
    Just t  -> do future <- getTodoUris n
                  return $ t : future
