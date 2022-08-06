module Network.URL.General

import Data.String
import Data.String.Parser
import Network.URL.Internal.StringParser
-- Data
public export
data GeneralURL  = MkGeneralURL String String

public export
Show GeneralURL where
  show (MkGeneralURL scheme specific) = "MkGeneralURL \"" ++ scheme ++ "\" \"" ++ specific ++ "\""

public export
Eq GeneralURL where
  (MkGeneralURL scheme specific) == (MkGeneralURL scheme1 specific1) = scheme == scheme1 && specific == specific1



private 
url : Parser GeneralURL
url = do
  scheme <- letters
  token ":"
  content <- takeWhile1 (\_ => True)
  eos
  pure $ MkGeneralURL scheme content

-- export function
public export
parse : String -> Maybe GeneralURL
parse str = case parse url str of
              Right (j,_) => Just j
              _ => Nothing

public export
stringify : GeneralURL -> String
stringify (MkGeneralURL scheme specific) = scheme ++ ":" ++ specific