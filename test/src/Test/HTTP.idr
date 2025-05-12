module Test.HTTP

import Tester
import Tester.Runner

import Network.URL.HTTP
import Network.URL.HTTP.Parser

||| 测试 HTTP URL 解析功能
private
testParser : List Test
testParser = [
    -- 不同主机名
    test "测试解析-主机名1" $ assertEq  (parse "http://www.baidu.com")  (Just $ MkHTTPURL "http" "www.baidu.com" Nothing "/" [] Nothing)
    , test "测试解析-主机名2" $ assertEq  (parse "http://localhost") (Just $ MkHTTPURL "http" "localhost" Nothing "/" [] Nothing)
    , test "测试解析-主机名3" $ assertEq  (parse "http://sub.example.com") (Just $ MkHTTPURL "http" "sub.example.com" Nothing "/" [] Nothing)
    -- 端口、路径、参数、锚点
    , test "测试解析-端口" $ assertEq  (parse "http://127.0.0.1:8080/")  (Just $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/" [] Nothing)
    , test "测试解析-路径" $ assertEq  (parse "http://127.0.0.1:8080/a/b.html") (Just $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/a/b.html" [] Nothing)
    , test "测试解析-参数" $ assertEq  (parse "http://127.0.0.1:8080/a/b.html?a=1&b=2") (Just $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/a/b.html" [("a","1"),("b","2")] Nothing)
    , test "测试解析-参数锚点" $ assertEq  (parse "http://127.0.0.1:8080/a/b.html?a=1&b=2#hello-world") (Just $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/a/b.html" [("a","1"),("b","2")] (Just "hello-world"))
    , test "测试解析-锚点" $ assertEq  (parse "http://127.0.0.1:8080/a/b.html#hello-world") (Just $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/a/b.html" [] (Just "hello-world"))
    -- 错误情况
    , test "测试解析-空URL" $ assertEq (parse "") Nothing
    , test "测试解析-无效协议字符" $ assertEq (parse "123://example.com") Nothing  -- 协议必须以字母开头
    , test "测试解析-无效端口" $ assertEq (parse "http://example.com:abc") Nothing
    , test "测试解析-端口范围" $ assertEq (parse "http://example.com:65536") Nothing  -- 端口号应该在 0-65535 范围内
    -- 协议多样性
    , test "测试解析-HTTPS协议" $ assertEq (parse "https://example.com") (Just $ MkHTTPURL "https" "example.com" Nothing "/" [] Nothing)
    , test "测试解析-FTP协议" $ assertEq (parse "ftp://example.com") (Just $ MkHTTPURL "ftp" "example.com" Nothing "/" [] Nothing)
    -- 特殊字符
    , test "测试解析-特殊字符路径" $ assertEq (parse "http://example.com/path%20with%20spaces") (Just $ MkHTTPURL "http" "example.com" Nothing "/path%20with%20spaces" [] Nothing)
]

||| 测试 HTTP URL 序列化功能
private
testStringify : List Test
testStringify = [
    test "测试编码1" $ assertEq  (stringify $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/a/b.html" [("a","1"),("b","2")] (Just "hello-world")) "http://127.0.0.1:8080/a/b.html?a=1&b=2#hello-world"
    , test "测试编码2" $ assertEq  (stringify $ MkHTTPURL "http" "127.0.0.1" (Just 8080) "/a/b.html" [] (Just "hello-world")) "http://127.0.0.1:8080/a/b.html#hello-world"
    -- 添加特殊情况测试
    , test "测试编码-无端口" $ assertEq (stringify $ MkHTTPURL "https" "example.com" Nothing "/" [] Nothing) "https://example.com/"
    , test "测试编码-空查询参数" $ assertEq (stringify $ MkHTTPURL "http" "example.com" Nothing "/path" [] Nothing) "http://example.com/path"
    , test "测试编码-多查询参数" $ assertEq (stringify $ MkHTTPURL "http" "example.com" Nothing "/" [("a","1"),("b","2"),("c","3")] Nothing) "http://example.com/?a=1&b=2&c=3"
]

private
mkUrlByParams : List QueryParam -> HTTPURL
mkUrlByParams qs = MkHTTPURL "https" "grass.show" Nothing "/" qs Nothing

||| 测试查询参数操作
private
testAddParams : List Test
testAddParams = [
    test "向空列表插入查询参数" $ assertEq test1 $ insertParam ("a","1") test0
    , test "向已有列表插入查询参数" $ assertEq test2 $ insertParam ("b","2") test1
    , test "向已有列表更新查询参数" $ assertEq test1 $ insertParam ("a","1") test11
    -- 添加更多参数操作测试
    , test "更新已存在的参数" $ assertEq (mkUrlByParams [("x","2")]) $ insertParam ("x","2") (mkUrlByParams [("x","1")])
    , test "添加多个参数" $ assertEq (mkUrlByParams [("a","1"),("b","2"),("c","3")]) $ 
        insertParam ("c","3") $ insertParam ("b","2") $ insertParam ("a","1") (mkUrlByParams [])
    , test "更新中间的参数" $ assertEq (mkUrlByParams [("a","1"),("b","3"),("c","2")]) $ 
        insertParam ("b","3") (mkUrlByParams [("a","1"),("b","2"),("c","2")])
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