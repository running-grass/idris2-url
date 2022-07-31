module Network.URL.HTTP.Parser

import Text.Lexer
import Text.Parser

import Data.List1
import Data.String
import Data.Maybe

import Network.URL.HTTP.Data


private
data HTTPTokenKind
  = Alpha | Digit | Dot 
    | Colon | Plus | Minus 
    | NumberSign | QuestionMark
    | Ampersand | EqualsSign
    | Slash | Other

private
Eq HTTPTokenKind where
  (==) Alpha Alpha = True
  (==) Digit Digit = True
  (==) Dot Dot = True
  (==) Colon Colon = True
  (==) Plus Plus = True
  (==) Minus Minus = True
  (==) Slash Slash = True
  (==) NumberSign NumberSign = True
  (==) QuestionMark QuestionMark = True
  (==) Ampersand Ampersand = True
  (==) EqualsSign EqualsSign = True
  (==) Other Other = True
  (==) _ _ = False

private
Show HTTPTokenKind where
  show Alpha = "Alpha"
  show Digit = "Digit"
  show Dot = "Dot"
  show Colon = "Colon"
  show Plus = "Plus"
  show Minus = "Minus"
  show Slash = "Slash"
  show NumberSign = "NumberSign"
  show QuestionMark = "QuestionMark"
  show Ampersand = "Ampersand"
  show EqualsSign = "EqualsSign"
  show Other = "Other"

private
TokenKind HTTPTokenKind where
  TokType Alpha = String
  TokType Digit = String
  TokType Dot = String
  TokType Colon = String
  TokType Plus = String
  TokType Minus = String
  TokType Slash = String
  TokType NumberSign = String
  TokType QuestionMark = String
  TokType Ampersand = String
  TokType EqualsSign = String
  TokType Other = String

  tokValue Alpha s = s
  tokValue Digit s = s
  tokValue Dot s = s
  tokValue Colon s = s
  tokValue Plus s = s
  tokValue Minus s = s
  tokValue Slash s = s
  tokValue NumberSign s = s
  tokValue QuestionMark s = s
  tokValue Ampersand s = s
  tokValue EqualsSign s = s
  tokValue Other s = s

private
HTTPToken : Type
HTTPToken = Token HTTPTokenKind

private
Show HTTPToken where
    show (Tok kind text) = "(" ++ show kind ++ " " ++ text ++ ")"


-- Lexer

private
tokenMap : TokenMap HTTPToken
tokenMap = toTokenMap [
  (is '.',  Dot),
  (is ':',  Colon),
  (is '+',  Plus),
  (is '-',  Minus),
  (is '/',  Slash),
  (is '?',  QuestionMark),
  (is '#',  NumberSign),
  (is '&',  Ampersand),
  (is '=',  EqualsSign),
  (digit,  Digit),
  (alpha,  Alpha),
  (any, Other)
]

private
lexer : String -> Maybe (List (WithBounds HTTPToken))
lexer str = 
  case lex tokenMap str of
    (tokens, _, _, "") => Just tokens
    _ => Nothing

-- parser

private
port : Grammar state HTTPToken True String
port = do
  _ <- match Colon
  p <- some $ match Digit
  pure $ joinBy "" $ forget p

private
parserPath : Grammar state HTTPToken True String
parserPath = do
  _ <- match Slash
  path1 <- many (match Slash 
              <|> match Alpha 
              <|> match Digit 
              <|> match Plus 
              <|> match Minus 
              <|> match Dot 
              <|> match Other)
  pure $ "/" ++  joinBy "" path1


private
parserQuery : Grammar state HTTPToken True (List QueryParam)
parserQuery = do
  _ <- match QuestionMark
  sepBy (match Ampersand) param

  where
    key : Grammar state HTTPToken False String
    key = do
      keys <- many (match Slash 
            <|> match Alpha
            <|> match Digit
            <|> match Plus
            <|> match Minus
            <|> match Dot
            <|> match Other
          )
      pure $ joinBy "" keys

    value : Grammar state HTTPToken False String  
    value = key

    param : Grammar state HTTPToken True QueryParam
    param = do
      k <- optional key
      _ <- match EqualsSign
      v <- optional value 

      pure (fromMaybe "" k, fromMaybe "" v)


private
parserFragment : Grammar state HTTPToken True String
parserFragment = do
  _ <- match NumberSign
  path1 <- many (match Slash 
              <|> match Alpha 
              <|> match Digit 
              <|> match Plus 
              <|> match Minus 
              <|> match Dot 
              <|> match Other)
  pure $ joinBy "" path1

private
parserScheme : Grammar state HTTPToken True String
parserScheme = do
  schemeHead <- match Alpha 
  schemeTail <- many (match Alpha <|> match Dot <|> match Plus <|> match Minus)
  _ <- match Colon
  pure $ schemeHead ++ joinBy "" schemeTail

private
parserHost : Grammar state HTTPToken True String
parserHost = do
  _ <- match Slash
  _ <- match Slash

  host1 <- some (match Alpha <|> match Digit <|> match Plus <|> match Minus <|> match Dot)
  pure $ joinBy "" (forget host1)

private
url : Grammar state HTTPToken True HTTPURL
url = do
  scheme <- parserScheme
  host <- parserHost
  port1 <- optional port
  path1 <- optional parserPath
  query1 <- optional parserQuery
  fragment1 <- optional parserFragment
  eof

  path <- pure $ (fromMaybe "/" path1)
  query <- pure $ (fromMaybe [] query1)

  pure $ MkHTTPURL scheme host port1 path query fragment1

private
parser : List (WithBounds HTTPToken) -> Maybe HTTPURL
parser toks = case parse url toks of
                      Right (j, []) => Just j
                      _ => Nothing

public export
parse : String -> Maybe HTTPURL
parse x = parser !(lexer x)

public export
stringify : HTTPURL -> String
stringify (MkHTTPURL scheme host port path query fragment) = scheme 
    ++ "://" ++ host ++ port' 
    ++ path ++ query' ++ fragment' 
  where 
    port' : String
    port' = case port of
      Nothing => ""
      (Just p) => ":" ++ p

    query' : String
    query' = case query of
      [] => ""
      xs => "?" ++ (joinBy "&" $ map (\(a,b) => a ++ "=" ++ b) xs)

    fragment' : String
    fragment' = case fragment of
      Nothing => ""
      (Just f) => "#" ++ f