
module ShowParser (parseShow) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

lexer = P.makeTokenParser emptyDef
parens = P.parens lexer
commaSep = P.commaSep lexer
braces = P.braces lexer
identifier = P.identifier lexer
symbol = P.symbol lexer
whiteSpace = P.whiteSpace lexer
stringLiteral = P.stringLiteral lexer
integer = P.integer lexer
brackets = P.brackets lexer

parseShow str = xml_header++(run_parser showParser str)

showParser :: Parser String
showParser =
  list_parser <|>
  tuple_parser <|>
  try record_parser <|>
  adt_parser <|>
  number <|>
  quoted_string <?>
  "Parse error"

run_parser :: Parser a -> String -> a
run_parser p str = case parse p "" str of
  Left err -> error $ "parse error at "++(show err)
  Right val -> val

xml_header =  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
otag t = "<"++t++">"
ctag t = "</"++t++">"
tag t v = concat [otag t, v, ctag t]

tagAttrs t attrs v =
  concat [
    otag (unwords $ [t]++(map(\(k, v) -> concat [k,"=\"",v,"\""]) attrs)),
    v,
    ctag t
    ]

joinNL = unlines 

list_parser = do
  ls <- brackets $ commaSep showParser
  return $ tag "list" $ joinNL $ map (tag "list-elt") ls

tuple_parser = do
  ls <- parens $ commaSep showParser
  return $ tag "tuple" $ unwords $ map (tag "tuple-elt") ls

record_parser = do
  ti <- type_identifier
  ls <- braces $ commaSep kvparser
  return $ tagAttrs "record" [("name",ti)] (joinNL ls)

kvparser = do
  k <- identifier
  symbol "="
  t <- showParser
  return $ tagAttrs "elt" [("key",k)] t

type_identifier = do
  fst <- oneOf ['A'..'Z']
  rest <- many alphaNum
  whiteSpace
  return $ fst:rest

adt_parser = do
  ti <- type_identifier
  return $ tag "adt" ti

quoted_string = do
  s <- stringLiteral
  return $ "\""++s++"\""

number = do
  n <- integer
  return $ show n

