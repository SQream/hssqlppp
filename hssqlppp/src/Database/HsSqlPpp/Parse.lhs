
Forward the public part of ParseInternal

> -- | Functions to parse SQL.

> module Database.HsSqlPpp.Parse
>     (-- * Main
>      parseStatements
>     ,parseQueryExpr
>     ,parseScalarExpr
>     ,parseProcSQL
>      -- * Parsing options
>     ,ParseFlags(..)
>     ,defaultParseFlags
>     ,Dialect(..)
>     ,ansiDialect
>      -- * errors
>     ,ParseErrorExtra(..)
>      -- * internals

TODO - export these from another module
will need lots more to support the extensions/chaos stuff

>     ,parseName
>     ,parseNameComponent

>     ) where
>
> import Database.HsSqlPpp.Internals.ParseInternal
> import Database.HsSqlPpp.Dialect
