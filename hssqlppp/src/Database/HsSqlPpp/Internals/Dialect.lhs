
> -- | The sql dialect type which is used by the parser and the pretty
> -- printer so has its own module.
> module Database.HsSqlPpp.Internals.Dialect where

more dialect options will be added here

> -- | The dialect of SQL to use.
> data Dialect = PostgreSQLDialect
>              | SQLServerDialect
>              | OracleDialect
>                deriving (Show,Eq)

data Dialect Source

Constructors
Dialect	 
diName :: StringdiSyntaxFlavour :: SyntaxFlavourdiCanonicalTypeNames :: [(Text, [Text])]diTextTypes :: [Text]diDatetimeTypes :: [Text]diNumberTypes :: [Text]namesForAnsiTypes :: [(Text, Text)]diDefaultCatalog :: Catalog
Instances
Eq DialectSource	 
Data DialectSource	 
Show DialectSource	 
data SyntaxFlavour Source

Constructors
Ansi	 
Postgres	 
SqlServer	 
Oracle	 
Instances
Eq SyntaxFlavourSource	 
Data SyntaxFlavourSource	 
Show SyntaxFlavourSource	 
ansiDialect :: Dialect Source

postgresDialect :: Dialect Source

sqlServerDialect :: Dialect Source

oracleDialect :: Dialect Source

canonicalizeTypeName :: Dialect -> Text -> Text Source

ansiTypeNameToDialect :: Dialect -> Text -> Maybe Text
