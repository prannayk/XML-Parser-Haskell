module ReadXML where

import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.String

type AttrName = String
type AttrVal = String

data Attribute = Attribute (AttrName,AttrVal) deriving (Eq,Show)
data XML = Element String [Attribute] [XML] | SelfClosingTag String [Attribute] | Decl String | Body String deriving (Show)

document = do
	spaces
	y <- (try xmlDecl <|> tag)
	spaces
	x <- many tag
	spaces
	return (y:x)

xmlDecl :: Parser XML
xmlDecl = do
	string "<?xml"
	spaces
	decl <- many (noneOf "?>")
	spaces
	string "?>"
	return (Decl decl)

tag = do
	char '<'
	spaces
	name <- many (letter <|> digit)
	spaces
	attr <- many attribute
	spaces
	close <- try (string "/>" <|> string ">")
	if (length close) == 2
	then return (SelfClosingTag name attr)
	else do 
		elementBody <- many elementBody
		string "</"
		spaces
		string name
		spaces
		string ">"
		spaces
		return (Element name attr elementBody)

elementBody = spaces *> try tag <|> text

text = Body <$> many1 (noneOf "><")

attribute = do
	name <- many (noneOf "= />")
	spaces
	char '='
	spaces
	char '"'
	value <- many (noneOf ['"'])
	char '"'
	spaces
	return (Attribute (name,value))

