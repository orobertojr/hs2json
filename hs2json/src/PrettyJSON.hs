module PrettyJSON
    (
      renderJValue
    ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))
import Prelude hiding ((<>))
import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text, string, compact, pretty)

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name,val) = string name
                          <> text ": "
                          <> renderJValue val

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
    where d = ord c

pointyString :: String -> Doc
pointyString s = enclose '"' '"' (hcat (map oneChar s))

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
<<<<<<< HEAD
                       . fsep . punctuate (char ',') . map item
<<<<<<< HEAD
                       
=======
=======
                       . fsep . punctuate (char ',') . map item
>>>>>>> 5e5cac3fe0d231bdb59f73c21de20b6a540622c3
>>>>>>> 6dffd07da0cfde202328759396b200e7aa5a780a
