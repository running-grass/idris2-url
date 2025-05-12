||| 通用 URL 处理模块
||| 提供基本的 URL 解析和字符串化功能
module Network.URL.General

import Data.String
import Data.String.Parser
import Network.URL.Internal.StringParser

||| 表示一个通用的 URL 结构
||| @scheme URL 的协议部分
||| @specific URL 的具体内容部分
public export
data GeneralURL = MkGeneralURL String String

||| 实现 Show 接口，用于将 URL 转换为字符串形式
public export
Show GeneralURL where
  show (MkGeneralURL scheme specific) = "MkGeneralURL \"" ++ scheme ++ "\" \"" ++ specific ++ "\""

||| 实现 Eq 接口，用于比较两个 URL 是否相等
public export
Eq GeneralURL where
  (MkGeneralURL scheme specific) == (MkGeneralURL scheme1 specific1) = scheme == scheme1 && specific == specific1

||| 内部解析器，用于解析 URL 字符串
private 
url : Parser GeneralURL
url = do
  scheme <- schemeParser     -- 解析协议部分（允许字母、数字、+、-、.，首字母为字母）
  token ":"            -- 解析冒号分隔符
  content <- takeWhile1 (\_ => True)  -- 解析剩余的所有内容
  eos                  -- 确保到达字符串末尾
  pure $ MkGeneralURL scheme content

||| 将字符串解析为 URL
||| @str 要解析的字符串
||| @returns 如果解析成功则返回 Just GeneralURL，否则返回 Nothing
public export
parse : String -> Maybe GeneralURL
parse str = case parse url str of
              Right (j,_) => Just j
              _ => Nothing

||| 将 URL 转换为字符串形式
||| @url 要转换的 URL
||| @returns URL 的字符串表示
public export
stringify : GeneralURL -> String
stringify (MkGeneralURL scheme specific) = scheme ++ ":" ++ specific