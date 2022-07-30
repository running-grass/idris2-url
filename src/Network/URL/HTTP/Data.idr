module Network.URL.HTTP.Data

-- Data
-- data HTTPURL  = MkHTTPURL String String
public export
record HTTPURL where
    constructor MkHTTPURL
    scheme, host, port, path, query : String

public export
Show HTTPURL where
  show (MkHTTPURL scheme host port path query) = "MkHTTPURL \"" ++ scheme ++ "\" \"" 
                                        ++ host ++ "\" \"" 
                                        ++ port ++ "\" \"" 
                                        ++ path ++ "\" \"" 
                                        ++ query ++ "\""

public export
Eq HTTPURL where
  (MkHTTPURL scheme host port path query) == (MkHTTPURL scheme1 host1 port1 path1 query1) = 
    scheme == scheme1 
    && host == host1
    && port == port1
    && path == path1
    && query == query1
