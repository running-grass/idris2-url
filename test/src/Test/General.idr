module Test.General

import Tester
import Tester.Runner

import Network.URL.General

-- 测试斜体
private
testParser : List Test
testParser = [
    test "General测试解析1" $ assertEq  (parse "file://c:/path/file")  (Just $ MkGeneralURL "file" "//c:/path/file")
    , test "General测试解析2" $ assertEq  (parse "mailto:467195537@qq.com")  (Just $ MkGeneralURL "mailto" "467195537@qq.com")
]

private
testStringify : List Test
testStringify = [
    test "General测试序列化1" $ assertEq  (stringify $ MkGeneralURL "file" "//c:/path/file") "file://c:/path/file"
    , test "General测试序列化2" $ assertEq  (stringify $ MkGeneralURL "mailto" "467195537@qq.com") "mailto:467195537@qq.com"
]

export 
tests : List Test
tests = testParser ++ testStringify

private
main : IO Bool
main = runTests tests