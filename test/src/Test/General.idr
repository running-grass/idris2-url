module Test.General

import Tester
import Tester.Runner

import Network.URL.General

||| 测试 URL 解析功能
private
testParser : List Test
testParser = [
    test "General测试解析1" $ assertEq  (parse "file://c:/path/file")  (Just $ MkGeneralURL "file" "//c:/path/file")
    , test "General测试解析2" $ assertEq  (parse "mailto:467195537@qq.com")  (Just $ MkGeneralURL "mailto" "467195537@qq.com")
    -- 添加错误情况测试
    , test "General测试解析-空字符串" $ assertEq (parse "") Nothing
    , test "General测试解析-无效scheme" $ assertEq (parse "123:abc") Nothing
    , test "General测试解析-缺少冒号" $ assertEq (parse "fileabc") Nothing
    , test "General测试解析-空specific" $ assertEq (parse "file:") Nothing
    -- 添加特殊情况测试
    , test "General测试解析-特殊字符" $ assertEq (parse "git:git@github.com") (Just $ MkGeneralURL "git" "git@github.com")
    , test "General测试解析-大写scheme" $ assertEq (parse "FILE:///etc/hosts") (Just $ MkGeneralURL "FILE" "///etc/hosts")
]

||| 测试 URL 序列化功能
private
testStringify : List Test
testStringify = [
    test "General测试序列化1" $ assertEq  (stringify $ MkGeneralURL "file" "//c:/path/file") "file://c:/path/file"
    , test "General测试序列化2" $ assertEq  (stringify $ MkGeneralURL "mailto" "467195537@qq.com") "mailto:467195537@qq.com"
    -- 添加特殊情况测试
    , test "General测试序列化-空specific" $ assertEq (stringify $ MkGeneralURL "file" "") "file:"
    , test "General测试序列化-特殊字符" $ assertEq (stringify $ MkGeneralURL "git" "git@github.com") "git:git@github.com"
]

||| 测试 URL 相等性比较
private
testEquality : List Test
testEquality = [
    test "General测试相等-完全相同" $ assertEq (MkGeneralURL "file" "//path") (MkGeneralURL "file" "//path")
    , test "General测试相等-不同scheme" $ assertEq False $ (MkGeneralURL "file" "//path") == (MkGeneralURL "http" "//path")
    , test "General测试相等-不同specific" $ assertEq False $ (MkGeneralURL "file" "//path1") == (MkGeneralURL "file" "//path2")
]

export 
tests : List Test
tests = testParser ++ testStringify ++ testEquality

private
main : IO Bool
main = runTests tests