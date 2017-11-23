

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.Identifiers
>     (identifiers) where

> import Database.HsSqlPpp.Tests.TestTypes
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Tests.TypeChecking.Utils


> identifiers :: Item
> identifiers =
>   Group "identifiers"
>   [ idnTest "select * from t0"
>       $ Right $ CompositeType [("a", mkTypeExtra typeBool)
>                               ,("A", mkTypeExtra typeInt)
>                               ,("a A", mkTypeExtra typeBigInt)]
>   , idnTest "select a from t0" $ Right $ CompositeType [("a", mkTypeExtra typeBool)]
>   , idnTest "select A from t0" $ Right $ CompositeType [("a", mkTypeExtra typeBool)]
>   , idnTest "select \"A\" from t0" $ Right $ CompositeType [("A", mkTypeExtra typeInt)]
>   , idnTest "select \"B\" from (select \"A\" as \"B\" from t0)"
>       $ Right $ CompositeType [("B", mkTypeExtra typeInt)]
>   , idnTest "select * from (select * from t0)"
>       $ Right $ CompositeType [("a", mkTypeExtra typeBool)
>                               ,("A", mkTypeExtra typeInt)
>                               ,("a A", mkTypeExtra typeBigInt)]
>   ]
>   where
>     idnTest sql ty = TCQueryExpr catalog sql ty
>     catalog = 
>       [ CatCreateTable ("public","t0")
>           [ ("a",mkCatNameExtra "bool")
>           , ("A",mkCatNameExtra "int4")
>           , ("a A",mkCatNameExtra "int8")
>           ]
>       ]
