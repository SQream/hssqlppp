
> -- | Hacky start on a separate catalog for tsql. At the moment, reuses the
> -- postgresql default template1 catalog and adds a few things.
> {-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
> module Database.HsSqlPpp.Internals.Catalog.DefaultTSQLCatalog
>      (defaultTSQLCatalog) where
>
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> import Database.HsSqlPpp.Internals.Catalog.DefaultTemplate1Catalog
> --import Data.List
> import Data.Generics.Uniplate.Data
> --import Debug.Trace
> --import Text.Show.Pretty

> defaultTSQLCatalog :: Catalog
> defaultTSQLCatalog = either (error . show) id catr
>   where
>     catr = updateCatalog
>               (alterUpdates (deconstructCatalog defaultTemplate1Catalog
>                              ++ additionalEntries))
>               defaultCatalog
>     -- change the counts to return int instead of long
>     alterUpdates = map $ \u -> case u of
>         CatCreateAggregate "count" ["any"] "int8" ->
>             CatCreateAggregate "count" ["any"] "int4"
>         CatCreateAggregate f [e] _ | f `elem` ["sum","avg"]
>                                    , e `elem` ["int1"
>                                               ,"int2"
>                                               ,"int4"] ->
>             CatCreateAggregate f [e] "int4"
>         CatCreateAggregate f ["float4"] _ | f `elem` ["sum","avg"] ->
>             CatCreateAggregate f ["float4"] "float8"
>         CatCreateAggregate f ["int8"] _ | f `elem` ["sum","avg"] ->
>             CatCreateAggregate f ["int8"] "int8"
>         _ -> u

>     additionalEntries =
>         int1fns ++
>         int12fns ++
>         [CatCreateScalarType "nvarchar"
>         ,CatCreateTypeCategoryEntry "nvarchar" ("S", False)
>         ,CatCreateBinaryOp "+" "varchar" "varchar" "varchar" True
>         ,CatCreateFunction "getdate" [] False "timestamp" True
>         ,CatCreateFunction "isnumeric" ["anyelement"] False "int4" True
>         ,CatCreateFunction "grt_lengthconv" ["int4"] False "int4" True
>         ,CatCreateFunction "isnull" ["anyelement","anyelement"] False "anyelement" True
>         -- put these in to stop use the text only version and a bunch of casts
>         ,CatCreateFunction "replace" ["char", "char", "char"] False "char" True
>         ,CatCreateFunction "replace" ["varchar", "varchar", "varchar"] False "varchar" True
>         ,CatCreateFunction "replace" ["nvarchar", "nvarchar", "nvarchar"] False "nvarchar" True
>         ,CatCreateFunction "patindex" ["char","char"] False "int4" True
>         ,CatCreateFunction "patindex" ["varchar","varchar"] False "int4" True
>         ,CatCreateFunction "patindex" ["nvarchar","nvarchar"] False "int4" True
>         ,CatCreateFunction "isdate" ["varchar"] False "bool" True
>         ,CatCreateFunction "isdate" ["char"] False "int4" True
>         ,CatCreateFunction "isdate" ["nvarchar"] False "int4" True
>         ,CatCreateFunction "len" ["nvarchar"] False "int4" True
>         ,CatCreateFunction "len" ["varchar"] False "int4" True
>         ,CatCreateAggregate "count_big" ["any"] "int8"
>         ,CatCreateFunction "datediff" ["int4","date","date"] False "int4" True
>         ,CatCreateFunction "datediff" ["int4","timestamp","timestamp"] False "int4" True
>         ,CatCreateFunction "dateadd" ["int4","int4","date"] False "date" True
>         ,CatCreateFunction "dateadd" ["int4","int4","timestamp"] False "timestamp" True
>         ,CatCreateFunction "datepart" ["int4","date"] False "int4" True
>         ,CatCreateFunction "datepart" ["int4","timestamp"] False "int4" True
>         ,CatCreateFunction "trunc" ["timestamp"] False "timestamp" True
>         ,CatCreateFunction "trunc" ["timestamp","int4"] False "timestamp" True
>         ,CatCreateFunction "trunc" ["date","int4"] False "date" True
>         ,CatCreateCast "char" "varchar" ImplicitCastContext

postponed until we have better design
  example of a problem: in "float4 < int4", both arguments are cast to text,
  because it has higher priority (see CatCreateTypeCategoryEntry)

>         --,CatCreateCast "int1" "varchar" ImplicitCastContext
>         --,CatCreateCast "int2" "varchar" ImplicitCastContext
>         --,CatCreateCast "int4" "varchar" ImplicitCastContext
>         --,CatCreateCast "int8" "varchar" ImplicitCastContext
>         --,CatCreateCast "float4" "varchar" ImplicitCastContext
>         --,CatCreateCast "float8" "varchar" ImplicitCastContext
>         --,CatCreateCast "int1" "text" ImplicitCastContext
>         --,CatCreateCast "int2" "text" ImplicitCastContext
>         --,CatCreateCast "int4" "text" ImplicitCastContext
>         --,CatCreateCast "int8" "text" ImplicitCastContext
>         --,CatCreateCast "float4" "text" ImplicitCastContext
>         --,CatCreateCast "float8" "text" ImplicitCastContext
>         ]
>     -- find all the functions on int2 and replace int2 with int1
>     -- then find all the functions with int2 and int4, and
>     -- replace int2 with int1 and int4 with int2
>     -- really hacky
>     int1fns = let s = filter (\x -> replaceItp x && hasInt2 x)
>                              (deconstructCatalog defaultTemplate1Catalog)
>               in flip transformBi s $ \x -> case (x :: CatName) of
>                                        "int2" -> "int1"
>                                        _ -> x
>     int12fns = let s = filter (\x -> replaceItp x && hasInt2Int4 x)
>                               (deconstructCatalog defaultTemplate1Catalog)
>                in flip transformBi s $ \x -> case (x :: CatName) of
>                                        "int2" -> "int1"
>                                        "int4" -> "int2"
>                                        _ -> x
>     hasInt2 x = not $ null [() | ("int2" :: CatName) <- universeBi x]
>     hasInt2Int4 x = not $ null [() | ("int2" :: CatName) <- universeBi x
>                                    , ("int4" :: CatName) <- universeBi x]
>     replaceItp x = case x of
>                      CatCreateScalarType {} -> True
>                      CatCreateArrayType {} -> True
>                      CatCreatePrefixOp {} -> True
>                      CatCreateBinaryOp {} -> True
>                      CatCreateFunction f _ _ _ _ | f `elem` ["abs","float4","float8","int2","int4","mod","numeric"] -> True
>                      CatCreateAggregate f _ _ | f `elem` ["avg","max","min","sum"] -> True
>                      CatCreateCast a b _ | a == "int2" || b == "int2" -> True
>                      CatCreateTypeCategoryEntry {} -> True
>                      _ -> False

comparisons with all ints
abs
cast functions
avg to numeric max min sum
catcreatecast: float4,float8, int4,int8,numeric, from

