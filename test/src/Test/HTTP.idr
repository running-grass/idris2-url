module Test.HTTP

import Tester
import Tester.Runner

import Network.URL.HTTP
import Network.URL.HTTP.Parser

-- 测试斜体
private
testParser : List Test
testParser = [
    test "测试解析1" $ assertEq  (parse "http://www.baidu.com")  (Just $ MkHTTPURL "http" "www.baidu.com" "" "" "")
    , test "测试解析2" $ assertEq  (parse "http://127.0.0.1:8080")  (Just $ MkHTTPURL "http" "127.0.0.1" "8080" "" "")
    , test "测试解析3" $ assertEq  (parse "http://127.0.0.1:8080/a/b.html") (Just $ MkHTTPURL "http" "127.0.0.1" "8080" "/a/b.html" "")
    , test "测试解析4" $ assertEq  (parse "http://127.0.0.1:8080/a/b.html?a=1&b=2") (Just $ MkHTTPURL "http" "127.0.0.1" "8080" "/a/b.html" "a=1&b=2")
]

-- private
-- testStringify : List Test
-- testStringify = [
--     test "测试解析1" $ assertEq  (stringify $ MkGeneralURL "file" "//c:/path/file") "file://c:/path/file"
--     , test "测试解析2" $ assertEq  (stringify $ MkGeneralURL "mailto" "467195537@qq.com") "mailto:467195537@qq.com"
-- ]

export 
tests : List Test
tests = testParser 

private
main : IO Bool
main = runTests tests