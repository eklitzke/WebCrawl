module WebCrawl.Parser where

import WebCrawl.Types

import System.IO.Unsafe

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Data.Maybe
import Data.List
import Text.HTML.TagSoup.Parser
import Text.HTML.TagSoup.Type
import Network.URI

{-
maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

-- e.g., listAfter "://" "http://foo.com/bar" -> Just "foo.com/bar"
listAfter :: Eq a => [a] -> [a] -> Maybe [a]
listAfter needle = fmap (drop $ length needle) . maybeHead . filter (isPrefixOf needle) . tails

-- Clean the path part of a URL. Right now this just strips the fragment, which
-- should only be interpreted by client agents (and not sent to the server in
-- requests).
cleanPath :: String -> String
cleanPath = fst . span (/= '#')

canonicalizeLink :: Domain -> String -> WebLocation
canonicalizeLink d p =
    case listAfter "://" p of
      Nothing -> mkHttpLoc d (cleanPath p)
      Just x  -> let (realDomain, path) = span (/= '/') x in
                 case path of
                   "" -> mkHttpLoc realDomain "/"
                   p' -> mkHttpLoc realDomain (cleanPath p')
-}

findLinks :: URI -> ByteString -> [URI]
findLinks uri body = catMaybes $ map fixUpURI links
    where
      body' = toString body
      atags = filter (isTagOpenName "a") (parseTags body')
      links = filter (/= "") $ map (fromAttrib "href") atags
      fixUpURI :: String -> Maybe URI
      fixUpURI u = do pu <- parseURIReference u
                      case uriScheme pu of
                        "" -> pu `relativeTo` uri
                        _  -> return pu
