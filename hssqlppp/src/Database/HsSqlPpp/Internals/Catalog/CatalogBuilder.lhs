
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Internals.Catalog.CatalogBuilder
>     (updateCatalog
>     ,deconstructCatalog
>     ,insertOperators
>     ,defaultCatalog
>     ) where

> import Control.Monad
> --import Data.List
> --import Data.Data
> --import Data.Char
> --import Data.Maybe

> import qualified Data.Map as M
> import qualified Data.Set as S
> import Database.HsSqlPpp.Internals.TypesInternal
> --import Database.HsSqlPpp.Utils.Utils
> --import Data.Text (Text)
> import qualified Data.Text as T
> --import qualified Data.Text.Lazy as LT
> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> import Database.HsSqlPpp.Internals.Catalog.CatalogUtils

> -- | Applies a list of 'CatalogUpdate's to an 'Catalog' value
> -- to produce a new Catalog value. TODO: there will be a split
> -- between the individual low level updates which just update
> -- one 'row' in the catalog type, and the high level updates
> -- which correspond to ddl (e.g. create type will also add the
> -- array type, create table will add a table, supply the
> -- private columns automatically, and add the composite type)
> -- highlevel not implemented yet
> updateCatalog :: [CatalogUpdate]
>               -> Catalog
>               -> Either [TypeError] Catalog
> updateCatalog eus cat' =
>   foldM updateCat' (cat' {catUpdates = catUpdates cat' ++ eus}) eus
>   where
>     updateCat' cat u = case u of
>       CatCreateSchema n ->
>         if S.member n (catSchemas cat)
>         then Left [SchemaAlreadyExists n]
>         else Right $ cat {catSchemas = S.insert n (catSchemas cat)}
>       CatCreateScalarType n ->
>         if S.member n (catScalarTypeNames cat)
>         -- todo: need to check all the type lists
>         -- and maybe need to check the name doesn't conflict with pseudo names or something?
>         -- this should happen with other cases as well
>         -- also: needs to take into account alias, so int and int4 are
>         -- both disallowed for new types, and lookup of either finds int4
>         then Left [InternalError $ "type already exists: " ++ show n]
>         else Right $ cat {catScalarTypeNames = S.insert n (catScalarTypeNames cat)}
>       CatCreateDomainType n b ->
>         Right $ cat {catDomainTypes = M.insert n b (catDomainTypes cat)}
>       CatCreateArrayType n b ->
>         Right $ cat {catArrayTypes = M.insert n b (catArrayTypes cat)}
>       -- todo: check the uniqueness of operator names (can overload by type)
>       -- also check the name of the operator is a valid operator name
>       -- and that the op has the correct number of args (1 or 2 resp.)
>       CatCreatePrefixOp n lt ret -> do
>         ltt <- catLookupType cat [QNmc $ T.unpack lt]
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         Right $ cat {catPrefixOps = insertOperators
>                                     [(n,(n,[ltt],rett,False))]
>                                     (catPrefixOps cat)}
>       CatCreatePostfixOp n rt ret -> do
>         rtt <- catLookupType cat [QNmc $ T.unpack rt]
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         Right $ cat {catPostfixOps = insertOperators
>                                      [(n,(n,[rtt],rett,False))]
>                                      (catPostfixOps cat)}
>       CatCreateBinaryOp n lt rt ret -> do
>         ltt <- catLookupType cat [QNmc $ T.unpack lt]
>         rtt <- catLookupType cat [QNmc $ T.unpack rt]
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         Right $ cat {catBinaryOps = insertOperators
>                                     [(n,(n,[ltt,rtt],rett,False))]
>                                     (catBinaryOps cat)}
>       CatCreateFunction n ps rs ret -> do
>         pst <- mapM (\nc -> catLookupType cat [QNmc $ T.unpack nc]) ps
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         let rett' = if rs
>                     then Pseudo $ SetOfType rett
>                     else rett
>         Right $ cat {catFunctions = insertOperators
>                                     [(n,(n,pst,rett',False))]
>                                     (catFunctions cat)}
>       CatCreateAggregate n ps ret -> do
>         pst <- mapM (\nc -> catLookupType cat [QNmc $ T.unpack nc]) ps
>         rett <- catLookupType cat [QNmc $ T.unpack ret]
>         Right $ cat {catAggregateFunctions = insertOperators
>                                     [(n,(n,pst,rett,False))]
>                                     (catAggregateFunctions cat)}
>       CatCreateTable n cs -> do
>         cts <- mapM (\(cn,te) -> do
>                        t' <- catLookupType cat [QNmc $ T.unpack $ catName te]
>                        -- for composite types, the information added here (about precision
>                        --   and nullability) is redundant
>                        let te' = TypeExtra t' (catPrecision te) (catScale te) (catNullable te)
>                        return (cn,te')) cs
>         Right $ cat {catTables = M.insert n (cts,[]) (catTables cat)}
>       CatCreateCast n0 n1 ctx -> do
>         t0 <- catLookupType cat [QNmc $ T.unpack n0]
>         t1 <- catLookupType cat [QNmc $ T.unpack n1]
>         Right $ cat {catCasts = S.insert (t0,t1,ctx) (catCasts cat)}
>       CatCreateTypeCategoryEntry n (c,p) -> do
>         t <- catLookupType cat [QNmc $ T.unpack n]
>         Right $ cat {catTypeCategories = M.insert t (c,p) $ catTypeCategories cat}

> deconstructCatalog :: Catalog -> [CatalogUpdate]
> deconstructCatalog = catUpdates

> insertOperators :: [(CatName,OperatorPrototype)]
>                 -> M.Map CatName [OperatorPrototype]
>                 -> M.Map CatName [OperatorPrototype]
> insertOperators vs m =
>   foldr i m vs
>   where
>     i (k,v) = M.insertWith (++) k [v]


'system' stuff

bunch of operators which you can use but don't appear in the
postgresql catalog

> -- | Represents what you probably want to use as a starting point if
> -- you are building an catalog from scratch. It contains
> -- information on built in function like things that aren't in the
> -- Postgres catalog, such as greatest, coalesce, keyword operators
> -- like \'and\', etc..
> defaultCatalog :: Catalog
> defaultCatalog =
>     -- todo: specify in terms of catalog updates
>   emptyCatalog {catSchemas = S.fromList ["public"]
>                ,catBinaryOps = insertOperators systemBinaryOps M.empty
>                ,catPrefixOps = insertOperators systemPrefixOps M.empty
>                ,catPostfixOps = insertOperators systemPostfixOps M.empty
>                ,catFunctions = insertOperators systemFunctions M.empty
>                ,catScalarTypeNames = rangeTypes}

> systemBinaryOps :: [(CatName,OperatorPrototype)]
> systemBinaryOps =
>    [("=", ("=",[Pseudo AnyElement, Pseudo AnyElement], typeBool, False))
>    ,("and",("and", [typeBool, typeBool], typeBool, False))
>    ,("or",("or", [typeBool, typeBool], typeBool, False))
>    ,("like",("like", [ScalarType "text", ScalarType "text"], typeBool, False))
>    ,("like",("like", [ScalarType "char", ScalarType "char"], typeBool, False))
>    ,("like",("like", [ScalarType "varchar", ScalarType "varchar"], typeBool, False))
>    ,("like",("like", [ScalarType "nvarchar", ScalarType "nvarchar"], typeBool, False))
>    ,("notlike",("notlike", [ScalarType "text", ScalarType "text"], typeBool, False))
>    ,("notlike",("notlike", [ScalarType "char", ScalarType "char"], typeBool, False))
>    ,("notlike",("notlike", [ScalarType "varchar", ScalarType "varchar"], typeBool, False))
>    ,("rlike",("rlike", [ScalarType "text", ScalarType "text"], typeBool, False))
>    ,("rlike",("rlike", [ScalarType "char", ScalarType "char"], typeBool, False))
>    ,("rlike",("rlike", [ScalarType "varchar", ScalarType "varchar"], typeBool, False))
>    ,("arrayctor",("arrayctor", [ArrayType $ Pseudo AnyElement], Pseudo AnyArray, True))
>    ,("between",("between", [Pseudo AnyElement
>                            ,Pseudo AnyElement
>                            ,Pseudo AnyElement], typeBool, False))
>    ,("notbetween",("mptbetween", [Pseudo AnyElement
>                                  ,Pseudo AnyElement
>                                  ,Pseudo AnyElement], typeBool, False))
>    ,("substring",("substring",[ScalarType "text",typeInt,typeInt],ScalarType "text",False))
>    ,("substring",("substring",[ScalarType "varchar",typeInt,typeInt],ScalarType "varchar",False))
>    ,("substring",("substring",[ScalarType "nvarchar",typeInt,typeInt],ScalarType "nvarchar",False))
>    ,("substring",("substring",[ScalarType "char",typeInt,typeInt],ScalarType "char",False))
>    ,("arraysub",("arraysub", [Pseudo AnyArray,typeInt], Pseudo AnyElement, False))
>    ]

> systemPrefixOps :: [(CatName,OperatorPrototype)]
> systemPrefixOps =
>    [("not",("not", [typeBool], typeBool, False))]

> systemPostfixOps :: [(CatName,OperatorPrototype)]
> systemPostfixOps =
>    [("isnull",("isnull", [Pseudo AnyElement], typeBool, False))
>    ,("isnotnull",("isnotnull", [Pseudo AnyElement], typeBool, False))]

> systemFunctions :: [(CatName, OperatorPrototype)]
> systemFunctions =
>  [("coalesce",("coalesce", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement, True))
>  ,("nullif", ("nullif",[Pseudo AnyElement, Pseudo AnyElement], Pseudo AnyElement,False))
>  ,("greatest",("greatest", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement,True))
>  ,("least",("least", [ArrayType $ Pseudo AnyElement], Pseudo AnyElement,True))
>  ]

built in range types in postgresql

todo: maybe these are in the catalog somewhere and should come from
postgres?

> rangeTypes :: S.Set CatName
> rangeTypes = S.fromList ["int4range", "int8range"
>                         ,"numrange","daterange"
>                         ,"tsrange","tstzrange"]
