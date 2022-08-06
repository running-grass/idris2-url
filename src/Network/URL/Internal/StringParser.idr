module Network.URL.Internal.StringParser

import Data.String.Parser
import Data.String
import Data.Fin

export
letters : Parser String
letters = do
  chars <- some $ letter
  pure . joinBy "" . map Data.String.singleton $ chars

export
digits : Parser String
digits = do 
  ds <- some $ digit
  pure . joinBy "" . map show $ ds

export
int : Parser Int
int = do
  ds <- digits
  pure $ cast ds