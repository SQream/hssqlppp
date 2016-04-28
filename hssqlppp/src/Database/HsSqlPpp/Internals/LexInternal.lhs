
> {-# LANGUAGE TupleSections,OverloadedStrings #-}
> module Database.HsSqlPpp.Internals.LexInternal
>     (Token(..)
>     ,prettyToken
>     ,lexToken
>     ,lexTokens
>     ) where

> --import qualified Data.Text as T
> import qualified Data.Text.Lazy as LT
> import Text.Parsec
> --Cimport Text.Parsec.String hdi
> import Text.Parsec.Text.Lazy
> import Control.Applicative hiding ((<|>), many)
> import Data.Char
> import Database.HsSqlPpp.Internals.Dialect
> import Control.Monad
> import Prelude hiding (takeWhile)
> import Data.Maybe

> -- | Represents a lexed token
> data Token
>     -- | a symbol in postgresql dialect is one of the following:
>     --
>     -- * one of the characters (),;[]{}  (the {} is for odbc)
>     --
>     -- * \'..\' or \':=\' or \'.\' or \':\'
>     --
>     -- * a compound symbol, which starts with one of \'*\/\<>=~!\@#%^&|\`?+-' and follows with 0 or more of '*\/<>=~!\@#%^&|\`?'
>     --
>     -- things that are not lexed as symbols:
>     --
>     -- * [] used in quoted identifiers, prefix \@,#,: used in identifiers
>     --
>     -- * $n positional arg
>     --
>     = Symbol String
>
>     -- | This is an identifier or keyword.
>     --
>     -- The 'Maybe (Char,Char)' selects the quoted style - 'Nothing' means the
>     -- identifier was unquoted
>     -- otherwise the two characters are the start and end quote.
>     --
>     -- \'\"\' is used to quote identifiers in standard sql, sql server also uses [brackets]
>     -- to quote identifiers.
>     --
>     -- The identifier also includes the \'variable marker prefix\'
>     -- used in sql server (e.g. \@identifier, #identifier), and oracle
>     -- (e.g. :identifier)
>     | Identifier (Maybe (Char,Char)) String
>
>     -- | This is a string literal.
>     --
>     -- The first field is the quotes used: single quote (\')
>     -- for normal strings, E' for escape supporting strings,
>     -- and $$ delimiter for postgresql dollar quoted strings.
>     --
>     -- The lexer doesn't process the escapes in strings, but passes
>     -- on the literal source e.g. E\'\\n\' parses to SqlString \"E\'\" \"\\n\"
>     -- with the literal characters \'\\\' and \'n\' in the string, not a newline character.
>     -- quotes within a string (\'\') or escaped string (\'\' or \\\') are passed through unchanged
>     | SqlString String String
>
>     -- | a number literal (integral or otherwise), stored in original format
>     -- unchanged
>     | SqlNumber String
>
>     -- | non-significant whitespace (space, tab, newline) (strictly speaking,
>     -- it is up to the client to decide whether the whitespace is significant
>     -- or not)
>     | Whitespace String
>
>     -- | a postgresql positional arg, e.g. $1
>     | PositionalArg Int
>
>     -- | a commented line using --, contains every character starting with the
>     -- \'--\' and including the terminating newline character if there is one
>     -- - this will be missing if the last line in the source is a line comment
>     -- with no trailing newline
>     | LineComment String
>
>     -- | a block comment, \/* stuff *\/, includes the comment delimiters
>     | BlockComment String
>
>     -- | an antiquotation splice, e.g. $x(stuff)
>     | Splice Char String
>
>     -- | the copy data in a copy from stdin
>     | CopyPayload String
>       deriving (Eq,Show)

> -- | Accurate pretty printing, if you lex a bunch of tokens,
> -- then pretty print them, should should get back exactly the
> -- same string
> prettyToken :: Dialect -> Token -> String
> prettyToken _ (Symbol s) = s
> prettyToken _ (Identifier Nothing t) = t
> prettyToken _ (Identifier (Just (a,b)) t) =
>     [a] ++ t ++ [b]
> prettyToken _ (SqlString "E'" t) = "E'" ++ t ++ "'"
> prettyToken _ (SqlString q t) = q ++ t ++ q
> prettyToken _ (SqlNumber r) = r
> prettyToken _ (Whitespace t) = t
> prettyToken _ (PositionalArg n) =  '$' : show n
> prettyToken _ (LineComment l) = l
> prettyToken _ (BlockComment c) = c
> prettyToken _ (Splice c t) =
>     '$':c:'(':t ++ ")"
> prettyToken _ (CopyPayload s) = s ++ "\\.\n"


not sure how to get the position information in the parse errors

TODO: try to make all parsers applicative only
investigate what is missing for postgresql
investigate differences for sql server, oracle, maybe db2 and mysql
  also

> lexTokens :: Dialect -> FilePath -> Maybe (Int,Int) -> LT.Text -> Either ParseError [((FilePath,Int,Int),Token)]
> lexTokens dialect fn' mp txt =
>     let (l',c') = fromMaybe (1,1) mp
>     in runParser (setPos (fn',l',c') *> many_p <* eof) () "" txt
>   where

pretty hacky, want to switch to a different lexer for copy from stdin
statements

if we see 'from stdin;' then try to lex a copy payload

>      many_p = some_p `mplus` return []
>      some_p = do
>        tok <- lexToken dialect
>        case tok of
>          (_, Identifier Nothing t) | map toLower t == "from" -> (tok:) <$> seeStdin
>          _ -> (tok:) <$> many_p
>      seeStdin = do
>        tok <- lexToken dialect
>        case tok of
>          (_,Identifier Nothing t) | map toLower t == "stdin" -> (tok:) <$> seeColon
>          (_,x) | isWs x -> (tok:) <$> seeStdin
>          _ -> (tok:) <$> many_p
>      seeColon = do
>        tok <- lexToken dialect
>        case tok of
>          (_,Symbol ";") -> (tok:) <$> copyPayload
>          _ -> (tok:) <$> many_p
>      copyPayload = do
>        p' <- getPosition
>        let pos = (sourceName p',sourceLine p', sourceColumn p')
>        tok <- char '\n' *>
>             ((\x -> (pos, CopyPayload $ x ++ "\n"))
>              <$> manyTill anyChar (try $ string "\n\\.\n"))
>        --let (_,CopyPayload t) = tok
>        --trace ("payload is '" ++ T.unpack t ++ "'") $ return ()
>        (tok:) <$> many_p
>      setPos (fn,l,c) = do
>         fmap (flip setSourceName fn
>                . flip setSourceLine l
>                . flip setSourceColumn c) getPosition
>           >>= setPosition
>      isWs :: Token -> Bool
>      isWs (Whitespace {}) = True
>      isWs (BlockComment {}) = True
>      isWs (LineComment {}) = True
>      isWs _ = False

> -- | parser for a sql token
> lexToken :: Dialect -> Parser ((FilePath,Int,Int),Token)
> lexToken d = do
>     p' <- getPosition
>     let p = (sourceName p',sourceLine p', sourceColumn p')
>     (p,) <$> choice [sqlString d
>                     ,identifier d
>                     ,lineComment d
>                     ,blockComment d
>                     ,sqlNumber d
>                     ,symbol d
>                     ,sqlWhitespace d
>                     ,positionalArg d
>                     ,splice d]

> identifier :: Dialect -> Parser Token

sql server: identifiers can start with @ or #
quoting uses [] or ""

TODO: fix all the "qiden" parsers to allow "qid""en"

> identifier (Dialect {diSyntaxFlavour = SqlServer}) =
>     choice
>     [Identifier (Just ('[',']'))
>      <$> (char '[' *> takeWhile1 (/=']') <* char ']')
>     ,Identifier (Just ('"','"'))
>      <$> (char '"' *> takeWhile1 (/='"') <* char '"')
>     ,Identifier Nothing <$> identifierStringPrefix '@'
>     ,Identifier Nothing <$> identifierStringPrefix '#'
>     ,Identifier Nothing <$> identifierString
>     ]

oracle: identifiers can start with :
quoting uses ""
(todo: check other possibilities)

> identifier (Dialect {diSyntaxFlavour = Oracle}) =
>     choice
>     [Identifier (Just ('"','"'))
>      <$> (char '"' *> takeWhile1 (/='"') <* char '"')
>     ,Identifier Nothing <$> identifierStringPrefix ':'
>     ,Identifier Nothing <$> identifierString
>     ]

> identifier (Dialect {diSyntaxFlavour = Postgres}) =
>     choice
>     [Identifier (Just ('"','"'))
>      <$> (char '"' *> takeWhile1 (/='"') <* char '"')
>     ,Identifier Nothing <$> identifierString
>     ]

> identifier (Dialect {diSyntaxFlavour = Ansi}) =
>     choice
>     [Identifier (Just ('"','"'))
>      <$> (char '"' *> takeWhile1 (/='"') <* char '"')
>     ,Identifier Nothing <$> identifierString
>     ]

> identifierStringPrefix :: Char  -> Parser String
> identifierStringPrefix p = do
>     void $ char p
>     i <- identifierString
>     return $  p:i

> identifierString :: Parser String
> identifierString =
>     startsWith (\c -> c == '_' || isAlpha c)
>                (\c -> c == '_' || isAlphaNum c)

Strings in sql:
postgresql dialect:
strings delimited with single quotes
a literal quote is written ''
the lexer leaves the double quote in the string in the ast
strings can also be written like this:
E'string with quotes in \n \t'
the \n and \t are escape sequences. The lexer passes these through unchanged.
an 'E' escaped string can also contain \' for a literal single quote.
this are also passed into the ast unchanged
strings can be dollar quoted:
$$string$$
the dollar quote can contain an optional tag:
$tag$string$tag$
which allows nesting of dollar quoted strings with different tags

Not sure what behaviour in sql server and oracle, pretty sure they
don't have dollar quoting, but I think they have the other two
variants.

> sqlString :: Dialect -> Parser Token
> sqlString _ =
>     choice [normalString
>            ,eString
>            ,dollarString]
>   where
>     normalString = SqlString "'" <$> (char '\'' *> normalStringSuffix "")
>     normalStringSuffix t = do
>         s <- takeTill (=='\'')
>         void $ char '\''
>         -- deal with '' as literal quote character
>         choice [do
>                 void $ char '\''
>                 normalStringSuffix $ concat [t,s,"''"]
>                ,return $ concat [t,s]]
>     eString = SqlString "E'" <$> (try (string "E'") *> eStringSuffix "")
>     eStringSuffix :: String -> Parser String
>     eStringSuffix t = do
>         s <- takeTill (`elem` ("\\'"::String))
>         choice [do
>                 try $ void $ string "\\'"
>                 eStringSuffix $ concat [t,s,"\\'"]
>                ,do
>                 void $ try $ string "''"
>                 eStringSuffix $ concat [t,s,"''"]
>                ,do
>                 void $ char '\''
>                 return $ concat [t,s]
>                ,do
>                 c <- anyChar
>                 eStringSuffix $ concat [t,s, [c]]]
>     dollarString = do
>         delim <- dollarDelim
>         y <- manyTill anyChar (try $ string delim)
>         return $ SqlString delim y
>     dollarDelim :: Parser String
>     dollarDelim = try $ do
>       void $ char '$'
>       tag <- option "" identifierString
>       void $ char '$'
>       return $ concat ["$", tag, "$"]

postgresql number parsing

digits
digits.[digits][e[+-]digits]
[digits].digits[e[+-]digits]
digitse[+-]digits
where digits is one or more decimal digits (0 through 9). At least one digit must be before or after the decimal point, if one is used. At least one digit must follow the exponent marker (e), if one is present. There cannot be any spaces or other characters embedded in the constant. Note that any leading plus or minus sign is not actually considered part of the constant; it is an operator applied to the constant.

> sqlNumber :: Dialect -> Parser Token
> sqlNumber _ = SqlNumber <$>
>     (int <??> (pp dot <??.> pp int)
>      -- try is used in case we read a dot
>      -- and it isn't part of a number
>      -- if there are any following digits, then we commit
>      -- to it being a number and not something else
>      <|> try ((++) <$> dot <*> int))
>     <??> pp expon
>   where
>     int = many1 digit
>     dot = do
>           -- make sure we don't parse '..' as part of a number
>           -- this is so we can parser e.g. 1..2 correctly
>           -- as '1', '..', '2', and not as '1.' '.2' or
>           -- '1.' '.' '2'
>           notFollowedBy (string "..")
>           string "."
>     expon = (:) <$> oneOf "eE" <*> sInt
>     sInt = (++) <$> option "" (string "+" <|> string "-") <*> int
>     pp = (<$$> (++))

> (<??>) :: Parser a -> Parser (a -> a) -> Parser a
> p <??> q = p <**> option id q

> (<??.>) :: Parser (a -> a) -> Parser (a -> a) -> Parser (a -> a)
> (<??.>) pa pb = (.) `c` pa <*> option id pb
>   -- todo: fix this mess
>   where c = (<$>) . flip

> (<$$>) :: Applicative f =>
>       f b -> (a -> b -> c) -> f (a -> c)
> (<$$>) pa c = pa <**> pure (flip c)

Symbols:

Copied from the postgresql manual:

An operator name is a sequence of up to NAMEDATALEN-1 (63 by default) characters from the following list:

+ - * / < > = ~ ! @ # % ^ & | ` ?

There are a few restrictions on operator names, however:
-- and /* cannot appear anywhere in an operator name, since they will be taken as the start of a comment.

A multiple-character operator name cannot end in + or -, unless the name also contains at least one of these characters:

~ ! @ # % ^ & | ` ?

For example, @- is an allowed operator name, but *- is not. This restriction allows PostgreSQL to parse SQL-compliant queries without requiring spaces between tokens.
When working with non-SQL-standard operator names, you will usually need to separate adjacent operators with spaces to avoid ambiguity. For example, if you have defined a left unary operator named @, you cannot write X*@Y; you must write X* @Y to ensure that PostgreSQL reads it as two operator names not one.

TODO: try to match this behaviour

inClass :: String -> Char -> Bool

> symbol :: Dialect -> Parser Token
> symbol dialect = Symbol <$>
>     choice
>     [(:[]) <$> satisfy (`elem` simpleSymbols)
>     ,try $ string ".."
>     ,string "."
>     ,try $ string "::"
>     ,try $ string ":="
>     ,string ":"
>     ,anotherOp
>     ]
>   where
>     anotherOp :: Parser String
>     anotherOp = do
>       -- first char can be any, this is always a valid operator name
>       c0 <- satisfy (`elem` compoundFirst)
>       --recurse:
>       let r = choice
>               [do
>                c1 <- satisfy (`elem` compoundTail)
>                choice [do
>                        x <- r
>                        return $ c1 : x
>                       ,return [c1]]
>               ,try $ do
>                a <- satisfy (`elem` ("+-"::String))
>                b <- r
>                return $ a : b]
>       choice [do
>               tl <- r
>               return $ c0 : tl
>              ,return [c0]]
>     {-biggerSymbol =
>         startsWith (inClass compoundFirst)
>                    (inClass compoundTail) -}
>     simpleSymbols :: String
>     simpleSymbols | diSyntaxFlavour dialect == Postgres = "(),;[]{}"
>                   | otherwise = "(),;{}"
>     compoundFirst :: String
>     compoundFirst | diSyntaxFlavour dialect == Postgres = "*/<>=~!@#%^&|`?+-"
>                   | otherwise = "*/<>=~!%^&|`?+-"
>     compoundTail :: String
>     compoundTail | diSyntaxFlavour dialect == Postgres = "*/<>=~!@#%^&|`?"
>                  | otherwise = "*/<>=~!%^&|`?"


> sqlWhitespace :: Dialect -> Parser Token
> sqlWhitespace _ = Whitespace <$> many1 (satisfy isSpace)

> positionalArg :: Dialect -> Parser Token
> -- uses try so we don't get confused with $splices
> positionalArg (Dialect {diSyntaxFlavour = Postgres}) = try (
>   PositionalArg <$> (char '$' *> (read <$> many1 digit)))

> positionalArg _ = satisfy (const False) >> fail "positional arg unsupported"

> lineComment :: Dialect -> Parser Token
> lineComment _ =
>     (\s -> LineComment$ concat ["--",s]) <$>
>     -- try is used here in case we see a - symbol
>     -- once we read two -- then we commit to the comment token
>     (try (string "--") *> (
>      conc <$> manyTill anyChar (lookAhead lineCommentEnd) <*> lineCommentEnd))
>   where
>     conc a Nothing = a
>     conc a (Just b) = a ++ b
>     lineCommentEnd = Just "\n" <$ char '\n' <|> Nothing <$ eof

> blockComment :: Dialect -> Parser Token
> blockComment _ =
>     (\s -> BlockComment $ concat ["/*",s]) <$>
>     (try (string "/*") *> commentSuffix 0)
>   where
>     commentSuffix :: Int -> Parser String
>     commentSuffix n = do
>       -- read until a possible end comment or nested comment
>       x <- takeWhile (\e -> e /= '/' && e /= '*')
>       choice [-- close comment: if the nesting is 0, done
>               -- otherwise recurse on commentSuffix
>               try (string "*/") *> let t = concat [x,"*/"]
>                                    in if n == 0
>                                       then return t
>                                       else (\s -> concat [t,s]) <$> commentSuffix (n - 1)
>               -- nested comment, recurse
>              ,try (string "/*") *> ((\s -> concat [x,"/*",s]) <$> commentSuffix (n + 1))
>               -- not an end comment or nested comment, continue
>              ,(\c s -> concat [x, [c], s]) <$> anyChar <*> commentSuffix n]

> splice :: Dialect -> Parser Token
> splice _ = do
>   Splice
>   <$> (char '$' *> letter)
>   <*> (char '(' *> identifierString <* char ')')

> startsWith :: (Char -> Bool) -> (Char -> Bool) -> Parser String
> startsWith p ps = do
>   c <- satisfy p
>   choice [(:) c <$> (takeWhile1 ps)
>          ,return $ [c]]

> takeWhile1 :: (Char -> Bool) -> Parser String
> takeWhile1 p = many1 (satisfy p)

> takeWhile :: (Char -> Bool) -> Parser String
> takeWhile p = many (satisfy p)

> takeTill :: (Char -> Bool) -> Parser String
> takeTill p = manyTill anyChar (peekSatisfy p)

> peekSatisfy :: (Char -> Bool) -> Parser ()
> peekSatisfy p = void $ lookAhead (satisfy p)
