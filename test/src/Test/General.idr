module Test.General

import Tester
import Tester.Runner

import Network.URL.General

-- 测试斜体
private
testParser : List Test
testParser = [
    test "测试解析1" $ assertEq  (parse "file://c:/path/file")  (Just $ MkGeneralURL "file" "//c:/path/file")
    , test "测试解析2" $ assertEq  (parse "mailto:467195537@qq.com")  (Just $ MkGeneralURL "mailto" "467195537@qq.com")
]

private
testStringify : List Test
testStringify = [
    test "测试解析1" $ assertEq  (stringify $ MkGeneralURL "file" "//c:/path/file") "file://c:/path/file"
    , test "测试解析2" $ assertEq  (stringify $ MkGeneralURL "mailto" "467195537@qq.com") "mailto:467195537@qq.com"
]

export 
tests : List Test
tests = testParser ++ testStringify

private
main : IO ()
main = do
    success <- runTests $ tests
    if success
        then putStrLn "通用UR解析L工作正常"
        else putStrLn "通用URL解析出错"