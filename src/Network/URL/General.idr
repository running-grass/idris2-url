module Network.URL.General

import Text.Lexer
import Text.Parser

import Data.List1
import Data.String

private
data GeneralTokenKind
  = Colon | Any

private
Eq GeneralTokenKind where
  (==) Colon Colon = True
  (==) Any Any = True
  (==) _ _ = False

private
Show GeneralTokenKind where
  show Colon = "Colon"
  show Any = "Any"

private
TokenKind GeneralTokenKind where
  TokType Colon = String
  TokType Any = String

  tokValue Colon _ = ":"
  tokValue Any s = s

private
GeneralToken : Type
GeneralToken = Token GeneralTokenKind

private
Show GeneralToken where
    show (Tok kind text) = "Tok kind: " ++ show kind ++ " text: " ++ text


-- Lexer


private
tokenMap : TokenMap GeneralToken
tokenMap = toTokenMap [
  (is ':',  Colon),
  (some $ isNot ':', Any)
]

private
lexer : String -> Maybe (List (WithBounds GeneralToken))
lexer str = 
  case lex tokenMap str of
    (tokens, _, _, "") => Just tokens
    _ => Nothing

-- Data
public export
data GeneralURL  = MkGeneralURL String String

public export
Show GeneralURL where
  show (MkGeneralURL scheme specific) = "MkGeneralURL \"" ++ scheme ++ "\" \"" ++ specific ++ "\""

public export
Eq GeneralURL where
  (MkGeneralURL scheme specific) == (MkGeneralURL scheme1 specific1) = scheme == scheme1 && specific == specific1

-- parser

private
url : Grammar state GeneralToken True GeneralURL
url = do
  scheme <- match Any
  _ <- match Colon
  specific <- some (match Any <|> match Colon)
  eof
  pure $ MkGeneralURL scheme $ foldr1 (++) specific

private
parser : List (WithBounds GeneralToken) -> Maybe GeneralURL
parser toks = case parse url toks of
                      Right (j, []) => Just j
                      _ => Nothing

public export
parse : String -> Maybe GeneralURL
parse x = parser !(lexer x)

public export
stringify : GeneralURL -> String
stringify (MkGeneralURL scheme specific) = scheme ++ ":" ++ specific