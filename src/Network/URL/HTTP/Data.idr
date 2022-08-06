module Network.URL.HTTP.Data

public export
QueryParam : Type
QueryParam = (String, String)

public export
record HTTPURL where
    constructor MkHTTPURL
    scheme, host : String
    port : Maybe Int
    path : String
    
    query : List QueryParam
    
    fragment : Maybe String

public export
Show HTTPURL where
  show (MkHTTPURL scheme host port path query fragment) = "MkHTTPURL \"" ++ scheme ++ "\" \"" 
                                        ++ host ++ "\" \"" 
                                        ++ show port ++ "\" \"" 
                                        ++ path ++ "\" \"" 
                                        ++ show query ++ "\" \"" 
                                        ++ show fragment ++ "\""

public export
Eq HTTPURL where
  (MkHTTPURL scheme host port path query fragment) == (MkHTTPURL scheme1 host1 port1 path1 query1 fragment1) = 
    scheme == scheme1 
    && host == host1
    && port == port1
    && path == path1
    && query == query1
    && fragment == fragment1

