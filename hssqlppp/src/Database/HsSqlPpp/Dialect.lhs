
> -- | The sql dialect type which is used by the parser and the pretty
> -- printer so has its own module.
> {-# LANGUAGE DeriveDataTypeable #-}
> module Database.HsSqlPpp.Dialect
>     (Dialect(..)
>     ,SyntaxFlavour(..)
>     ,ansiDialect
>     ,postgresDialect
>     ,sqlServerDialect
>     ,oracleDialect
>     ,canonicalizeTypeName
>     ,ansiTypeNameToDialect
>     ) where

> import Database.HsSqlPpp.Internals.Dialect
> import Database.HsSqlPpp.Dialects.DefaultTSQLCatalog
> --import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> import Database.HsSqlPpp.Dialects.DefaultTemplate1Catalog
> --import Data.Data
> import Data.Text (Text)

> ansiDialect :: Dialect
> ansiDialect = emptyDialect {diName = "ansi"
>                            ,diSyntaxFlavour=Postgres
>                            ,diDefaultCatalog = defaultTemplate1Catalog}

> sqlServerDialect :: Dialect
> sqlServerDialect = emptyDialect {diName = "sqlserver"
>                                 ,diSyntaxFlavour=SqlServer
>                                 ,diDefaultCatalog = defaultTSQLCatalog}

> oracleDialect :: Dialect
> oracleDialect = emptyDialect {diName = "oracle"
>                              ,diSyntaxFlavour=Oracle
>                              ,diDefaultCatalog = defaultTemplate1Catalog}

> ansiTypeNameToDialect :: Dialect -> Text -> Maybe Text
> ansiTypeNameToDialect = error "ansiTypeNameToDialect not supported in this version"

> canonicalizeTypeName :: Dialect -> Text -> Text
> canonicalizeTypeName = error "canonicalizeTypeName not supported in this version"
