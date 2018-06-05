
Forward the public part of ParserInternal

> -- | Functions to parse SQL.

> module Database.HsSqlPpp.Parser
>     (-- * Main
>      parseStatements
>     ,parseQueryExpr
>     ,parseScalarExpr
>     ,parsePlpgsql
>      -- * Parsing options
>     ,ParseFlags(..)
>     ,defaultParseFlags
>     ,SQLSyntaxDialect(..)
>      -- * errors
>     ,ParseErrorExtra(..)
>      -- * internals

TODO - export these from another module
will need lots more to support the extensions/chaos stuff

>     ,parseName
>     ,parseNameComponent

>     ,simpleParseStatements
>     ) where
>
> import Database.HsSqlPpp.Parsing.ParserInternal
> import Data.Text.Lazy (pack)
> import Text.Show.Pretty

> simpleParseStatements =
>   either (error . ppShow) (putStrLn . ppShow)
>   . parseStatements defaultParseFlags "simple" Nothing
>   . pack


