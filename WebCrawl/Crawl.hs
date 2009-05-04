module WebCrawl.Crawl where

import qualified Data.ByteString as BS

import WebCrawl.Globals
import WebCrawl.Util
import WebCrawl.Parser

import GHC.Conc
import Control.Concurrent.Chan
import Data.Maybe
import Network.URI
import Network.HTTP hiding (mkRequest)
import Network.HTTP.Base
import Network.BufferType
import Network.Stream

-- the headers that are sent by default
myHeaders :: [Header]
myHeaders = map (uncurry mkHeader) [ (HdrUserAgent, "webcrawl-hs")
                                   , (HdrConnection, "close") ]
visitURI :: Int -> URI -> IO ()
visitURI n uri = do
  putStrLn $ "visitURI " ++ (show uri)
  res <- simpleHTTP (mkRequest GET uri)
  bs <- getResponseBody res
  writeChan visitedChan (uri, bs)
  writeList2Chan todoChan [(n, l) | l <- findLinks uri bs]
  return ()

addCrawlTarget :: String -> IO ()
addCrawlTarget target = do
  let uri = fromJust $ parseAbsoluteURI target
  writeChan todoChan (-1, uri)


crawlWeb :: Int -> IO ()
crawlWeb n = crawlWeb' n 0
    where
      crawlWeb' 0 c  = return ()
      crawlWeb' n' c = do
        putStrLn $ "crawling web, n = " ++ (show n')
        todo <- getTodoUris c
        case todo of
          [] -> return ()
          _  -> do putStrLn $ "Working on " ++ (show todo)
                   mapM_ (visitURI c) todo
                   -- wait
                   crawlWeb' (n'-1) (c+1)
