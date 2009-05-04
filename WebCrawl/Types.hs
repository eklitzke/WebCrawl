module WebCrawl.Types where

import Data.Bits 

type Domain = String
type Flags = Int

hasFlags :: Flags -> Flags -> Bool
hasFlags thing test = thing .&. test == test
