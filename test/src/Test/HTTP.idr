module Test.HTTP

import Tester
import Tester.Runner

import Network.URL.HTTP
import Network.URL.HTTP.Parser

-- 测试斜体
private
testParser : List Test
testParser = [
    test "测试解析1" $ assertEq  (parse "http://www.baidu.com")  (Just $ MkHTTPURL "http" "www.baidu.com" Nothing "/" [] Nothing)
    , test "测试解析2" $ assertEq  (parse "http://127.0.0.1:8080/")  (Just $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/" [] Nothing)
    , test "测试解析3" $ assertEq  (parse "http://127.0.0.1:8080/a/b.html") (Just $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/a/b.html" [] Nothing)
    , test "测试解析4" $ assertEq  (parse "http://127.0.0.1:8080/a/b.html?a=1&b=2") (Just $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/a/b.html" [("a","1"),("b","2")] Nothing)
    , test "测试解析5" $ assertEq  (parse "http://127.0.0.1:8080/a/b.html?a=1&b=2#hello-world") (Just $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/a/b.html" [("a","1"),("b","2")] (Just "hello-world"))
    , test "测试解析6" $ assertEq  (parse "http://127.0.0.1:8080/a/b.html#hello-world") (Just $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/a/b.html" [] (Just "hello-world"))
]

private
testStringify : List Test
testStringify = [
    test "测试编码1" $ assertEq  (stringify $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/a/b.html" [("a","1"),("b","2")] (Just "hello-world")) "http://127.0.0.1:8080/a/b.html?a=1&b=2#hello-world"
    , test "测试编码2" $ assertEq  (stringify $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/a/b.html" [] (Just "hello-world")) "http://127.0.0.1:8080/a/b.html#hello-world"
]

private
mkUrlByParams : List QueryParam -> HTTPURL
mkUrlByParams qs = MkHTTPURL "https" "grass.show" Nothing "/" qs Nothing

private
testAddParams : List Test
testAddParams = [
    test "向空列表插入查询参数" $ assertEq test1 $ insertParam ("a","1") test0
    , test "向已有列表插入查询参数" $ assertEq test2 $ insertParam ("b","2") test1
    , test "向已有列表更新查询参数" $ assertEq test1 $ insertParam ("a","1") test11
]
    where
        test0 = mkUrlByParams []
        test1 = mkUrlByParams [("a","1")]
        test11 = mkUrlByParams [("a","11")]
        test2 = mkUrlByParams [("a","1"), ("b","2")]

export 
tests : List Test
tests = testParser ++ testStringify ++ testAddParams

private
main : IO Bool
main = runTests tests