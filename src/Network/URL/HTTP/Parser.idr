module Network.URL.HTTP.Parser

import Text.Lexer
import Text.Parser


import Data.List1
import Data.List
import Data.String
import Data.Maybe

import Network.URL.HTTP.Data
import Network.URL.Internal.StringParser
import Network.URL.Internal.Predicate
import Data.String.Parser

private
validaId : Char -> Bool
validaId = anyPass [isAlphaNum, (== '+'), (== '-'), (== '.')]

private 
parserScheme : Parser String
parserScheme = do
  schemeHead <- letter
  schemeTail <- takeWhile validaId
  token ":"
  pure $ singleton schemeHead ++ schemeTail

private
parserHost : Parser String
parserHost = do
  token "//"
  takeWhile1 validaId

||| 验证端口号是否在有效范围内 (0-65535)
private
validatePort : Int -> Maybe Int
validatePort p = if p >= 0 && p <= 65535 then Just p else Nothing

private
parserPort : Parser Int
parserPort = do
  token ":"
  p <- int
  case validatePort p of
    Just validPort => pure validPort
    Nothing => fail "Port number must be between 0 and 65535"

private
parserPathItem : Parser String
parserPathItem = do
  token "/"
  s <- takeWhile $ allPass [(/= '/'), (/= '?'), (/= '#')]
  pure $ "/" ++ s

private
parserPath : Parser String
parserPath = do
  s <- some parserPathItem
  pure $ joinBy "" s

private
parserQueryItem : Parser QueryParam
parserQueryItem = do
  s <- sepBy1 (takeWhile $ allPass [(/= '#'), (/= '&'), (/= '=')]) (token "=")
  case s of
    (k ::: Nil) => pure (k, "")
    (k ::: vs) => pure (k, joinBy "" vs)
  

private
parserQuery : Parser (List QueryParam)
parserQuery = do
  token "?"
  l <- sepBy1 parserQueryItem $ token "&"
  pure $ forget l

private
parserFragment : Parser String
parserFragment = do
  token "#"
  takeWhile (\t => True)

private 
url : Parser HTTPURL
url = do
  scheme <- parserScheme
  content <- parserHost
  port <- optional parserPort
  path <- option "/" parserPath
  query <- option [] parserQuery
  fragment <- optional parserFragment
  eos
  pure $ MkHTTPURL scheme content port path query fragment

-- export function
public export
parse : String -> Maybe HTTPURL
parse str = case parse url str of
              Right (j,_) => Just j
              _ => Nothing
              
public export
stringify : HTTPURL -> String
stringify (MkHTTPURL scheme host port path query fragment) = scheme 
    ++ "://" ++ host ++ port' 
    ++ path ++ query' ++ fragment' 
  where 
    port' : String
    port' = case port of
      Nothing => ""
      (Just p) => ":" ++ show p

    query' : String
    query' = case query of
      [] => ""
      xs => "?" ++ (joinBy "&" $ map (\(a,b) => a ++ "=" ++ b) xs)

    fragment' : String
    fragment' = case fragment of
      Nothing => ""
      (Just f) => "#" ++ f