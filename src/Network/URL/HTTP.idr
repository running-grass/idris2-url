module Network.URL.HTTP

import public Network.URL.HTTP.Data

import Data.List
import Data.Fin

public export
insertParam : QueryParam -> HTTPURL -> HTTPURL
insertParam p@(key, value) url = { query := newQuery } url
  where
    params : List QueryParam
    params = url.query

    hasIn : Maybe (Fin (Prelude.List.length $ url.query))
    hasIn = findIndex (\(k,_) => k == key) params

    update : Fin (length (url .query)) -> List QueryParam -> List QueryParam
    update idx ps = case inBounds (cast idx) ps of
       (Yes _) => replaceAt (cast idx) p ps
       _ => ps

    newQuery : List QueryParam 
    newQuery = case hasIn of
      Nothing => snoc params p
      (Just idx) => update idx params
