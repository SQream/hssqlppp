
Forward the public part of ParserInternal

> -- | Functions to parse SQL.

> module Database.HsSqlPpp.Parse
>     (-- * Main
>      parseStatements
>     ,parseQueryExpr
>     ,parseScalarExpr
>     ,parsePlpgsql
>      -- * Parsing options
>     ,ParseFlags(..)
>     ,defaultParseFlags
>     ,module Database.HsSqlPpp.Dialect
>      -- * errors
>     ,ParseErrorExtra(..)
>      -- * internals

TODO - export these from another module
will need lots more to support the extensions/chaos stuff

>     ,parseName
>     ,parseNameComponent

>     ) where
>
> import Database.HsSqlPpp.Internals.ParserInternal
> import Database.HsSqlPpp.Dialect
