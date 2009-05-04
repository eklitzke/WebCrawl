module WebCrawl.Globals where

import WebCrawl.Types
import System.IO.Unsafe
import Network.URI
import GHC.Conc
import Control.Concurrent.Chan
import Data.ByteString

visitedChan :: Chan (URI, ByteString)
visitedChan = unsafePerformIO $ newChan

todoChan :: Chan (Int, URI)
todoChan = unsafePerformIO $ newChan
