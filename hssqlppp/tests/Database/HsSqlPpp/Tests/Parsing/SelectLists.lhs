> {-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Tests.Parsing.SelectLists (selectLists) where
>
> --import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

> selectLists :: Item
> selectLists =
>    Group "selectLists"
>    [q "select a from tbl" stbl
>    ,q "select a,b from tbl"
>       $ stbl {selSelectList = sl [si $ ei "a"
>                                  ,si $ ei "b"]}
>    ,q "select a as b from tbl"
>       $ stbl {selSelectList = sl [sia (ei "a") $ Nmc "b"]}
>    ,q "select * from tbl"
>       $ stbl {selSelectList = sl [si $ Star ea]}
>    ,q "select tbl.* from tbl"
>       $ stbl {selSelectList = sl [si $ QStar ea (Nmc "tbl")]}
>    ,q "select a + b as b from tbl;"
>       $ stbl {selSelectList = sl [sia (binop "+" (ei "a") (ei "b")) $ Nmc "b"]}

>    -- window stuff
>    ,q "select row_number() over(order by a) as place from tbl;"
>       $ stbl
>         {selSelectList =
>           sl [sia (WindowApp ea
>                     (app "row_number" [])
>                     []
>                     [(ei "a", Asc, NullsDefault)] Nothing)
>               $ Nmc "place"]}
>    ,q "select row_number() over(order by a asc) as place from tbl;"
>       $ stbl
>         {selSelectList =
>           sl [sia (WindowApp ea
>                     (app "row_number" [])
>                     []
>                     [(ei "a", Asc, NullsDefault)] Nothing)
>               $ Nmc "place"]}
>    ,q "select row_number() over(order by a desc) as place from tbl;"
>       $ stbl
>         {selSelectList =
>           sl [sia (WindowApp ea
>                     (app "row_number" [])
>                     []
>                     [(ei "a", Desc, NullsDefault)] Nothing)
>               $ Nmc "place"]}
>    ,q "select row_number()\n\
>         \over(partition by a,b order by c) as place\n\
>         \from tbl;"
>       $ stbl
>         {selSelectList = sl [sia (WindowApp ea
>                                   (app "row_number" [])
>                                   [ei "a", ei "b"]
>                                   [(ei "c", Asc, NullsDefault)]
>                                   Nothing)
>                              (Nmc "place")]}
>    ,q "select row_number() over(), x from tbl;"
>       $ stbl
>         {selSelectList = sl [si $ WindowApp ea
>                                   (app "row_number" [])
>                                   [] [] Nothing
>                             ,si $ ei "x"]}

aggregates, group by, having

>    ,q "select count(distinct b) from tbl;"
>       $ stbl {selSelectList =
>               sl [si $ AggregateApp ea Distinct (app "count" [ei "b"]) []]}
>    ,q "select count(all b) from tbl;"
>       $ stbl {selSelectList =
>               sl [si $ AggregateApp ea All (app "count" [ei "b"]) []]}
>    ,q "select string_agg(distinct relname,',' order by relname1) from tbl;"
>       $ stbl {selSelectList =
>               sl [si $ AggregateApp ea Distinct
>                   (app "string_agg" [ei "relname",str ","])
>                   [(ei "relname1", Asc, NullsDefault)]]}
>    ,q "select a, count(b) from tbl group by a;"
>       $ stbl {selSelectList =
>               sl [si $ ei "a"
>                  ,si $ app "count" [ei "b"]]
>              ,selGroupBy = [ei "a"]}
>    ,q "select a, count(b) as cnt from tbl group by a having cnt > 4;"
>       $ stbl {selSelectList =
>               sl [si $ ei "a"
>                  ,sia (app "count" [ei "b"]) $ Nmc "cnt"]
>              ,selGroupBy = [ei "a"]
>              ,selHaving = Just $ binop  ">" (ei "cnt") (num "4")}
>    ,q "select a b from tbl;"
>       $ stbl { selSelectList = sl [sia (ei "a") (Nmc "b")] }
>    ,q "select a b, b c, c d from tbl;"
>       $ stbl { selSelectList = sl [sia (ei "a") (Nmc "b"), sia (ei "b") (Nmc "c"), sia (ei "c") (Nmc "d")] }
>    ,q "select a + b b from tbl;"
>       $ stbl {selSelectList = sl [sia (binop "+" (ei "a") (ei "b")) $ Nmc "b"]}
>    ,q "select row_number() over(order by a) place from tbl;"
>       $ stbl
>         {selSelectList =
>           sl [sia (WindowApp ea
>                     (app "row_number" [])
>                     []
>                     [(ei "a", Asc, NullsDefault)] Nothing)
>               $ Nmc "place"]}
>    ]
>    where
>      stbl = makeSelect
>             {selSelectList = sl [si $ ei "a"]
>             ,selTref = [tref "tbl"]}
>      q = QueryExpr
