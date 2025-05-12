||| HTTP URL 处理模块
||| 提供 HTTP URL 相关的操作功能
module Network.URL.HTTP

import public Network.URL.HTTP.Data

import Data.List
import Data.Fin

||| 向 HTTP URL 中插入或更新查询参数
||| @p 要插入的查询参数（键值对）
||| @url 目标 URL
||| @returns 更新后的 URL
public export
insertParam : QueryParam -> HTTPURL -> HTTPURL
insertParam p@(key, value) url = { query := newQuery } url
  where
    -- 获取当前的查询参数列表
    params : List QueryParam
    params = url.query

    -- 查找是否已存在相同键的参数
    hasIn : Maybe (Fin (Prelude.List.length $ url.query))
    hasIn = findIndex (\(k,_) => k == key) params

    -- 更新指定位置的参数
    update : Fin (length (url .query)) -> List QueryParam -> List QueryParam
    update idx ps = case inBounds (cast idx) ps of
       (Yes _) => replaceAt (cast idx) p ps
       _ => ps

    -- 生成新的查询参数列表
    -- 如果参数已存在则更新，否则添加到末尾
    newQuery : List QueryParam 
    newQuery = case hasIn of
      Nothing => snoc params p
      (Just idx) => update idx params
