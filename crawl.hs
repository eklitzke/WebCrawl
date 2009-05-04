import WebCrawl.Parser
import WebCrawl.Crawl
import WebCrawl.Globals
import Network.URI
import Control.Concurrent.Chan
import System.Environment
import qualified Data.ByteString as BS

main = do args <- getArgs
          if length args /= 1
             then putStrLn "must have exactly one arg"
             else do addCrawlTarget (head args)
                     crawlWeb 3
