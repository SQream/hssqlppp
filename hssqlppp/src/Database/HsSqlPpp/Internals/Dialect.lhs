
> -- | The sql dialect type which is used by the parser and the pretty
> -- printer so has its own module.
> {-# LANGUAGE DeriveDataTypeable #-}
> module Database.HsSqlPpp.Internals.Dialect where

> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> import Data.Data
> import Data.Text (Text)

stub for forward compatibility. This version of the code just looks at
the syntax flavour

> data SyntaxFlavour = Ansi | Postgres | SqlServer | Oracle
>                      deriving (Eq,Show,Data,Typeable)


> data Dialect = Dialect
>      {diName :: String
>      ,diSyntaxFlavour :: SyntaxFlavour
>      ,diCanonicalTypeNames :: [(Text,[Text])]
>      ,diTextTypes :: [Text] -- names of the text types (canonical names must be used)
>      ,diDatetimeTypes :: [Text]
>      ,diNumberTypes :: [Text] -- names of the number types (canonical names must be used)
>      -- this is a map from the canonical ansi name (in hssqlppp)
>      -- to the canonical name in the dialect
>      -- if there is no entry, then it means that type isn't
>      -- supported in this dialect
>      ,namesForAnsiTypes :: [(Text,Text)]
>      ,diDefaultCatalog :: Catalog
>      } deriving (Eq,Show,Data,Typeable)

> emptyDialect :: Dialect
> emptyDialect = Dialect {diName = ""
>                        ,diSyntaxFlavour = Postgres
>                        ,diCanonicalTypeNames = []
>                        ,diTextTypes = []
>                        ,diDatetimeTypes = []
>                        ,diNumberTypes = []
>                        ,namesForAnsiTypes = []
>                        ,diDefaultCatalog = emptyCatalog}


> postgresDialect :: Dialect
> postgresDialect = emptyDialect {diName = "postgres", diSyntaxFlavour=Postgres}
