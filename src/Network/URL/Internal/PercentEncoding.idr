module Network.URL.Internal.PercentEncoding

import Data.String
import Prelude
import Data.List

||| 判断字符是否为URL中无需编码的unreserved字符
||| 参考RFC 3986: ALPHA / DIGIT / "-" / "." / "_" / "~"
public export
isUnreserved : Char -> Bool
isUnreserved c = isAlphaNum c || c == '-' || c == '_' || c == '.' || c == '~'

-- 将整数转为两位十六进制字符串（大写）
private
toHex : Int -> String
toHex n = let hex = unpack "0123456789ABCDEF"
              hi  = n `div` 16
              lo  = n `mod` 16
          in  singleton (index hex (cast hi)) ++ singleton (index hex (cast lo))
  where
    index : List Char -> Nat -> Char
    index xs i = case drop i xs of
      (x :: _) => x
      _ => '0'

-- 将两个十六进制字符转为整数
private
fromHex : Char -> Char -> Int
fromHex a b = hexVal a * 16 + hexVal b
  where
    hexVal : Char -> Int
    hexVal c = case c of
      '0' => 0
      '1' => 1
      '2' => 2
      '3' => 3
      '4' => 4
      '5' => 5
      '6' => 6
      '7' => 7
      '8' => 8
      '9' => 9
      'A' => 10
      'B' => 11
      'C' => 12
      'D' => 13
      'E' => 14
      'F' => 15
      'a' => 10
      'b' => 11
      'c' => 12
      'd' => 13
      'e' => 14
      'f' => 15
      _   => 0 -- 非法字符按0处理

||| URL编码（百分号编码）实现
||| 将非unreserved字符转为%XX格式
public export
percentEncode : String -> String
percentEncode s = concatMap encodeChar (unpack s)
  where
    encodeChar : Char -> String
    encodeChar c = if isUnreserved c then singleton c else "%" ++ toHex (ord c)

||| URL解码（百分号解码）实现
||| 将%XX格式还原为原字符
public export
percentDecode : String -> String
percentDecode s = decode (unpack s)
  where
    decode : List Char -> String
    decode [] = ""
    decode ('%' :: a :: b :: xs) =
      let n = fromHex a b in singleton (chr n) ++ decode xs
    decode (x :: xs) = singleton x ++ decode xs 