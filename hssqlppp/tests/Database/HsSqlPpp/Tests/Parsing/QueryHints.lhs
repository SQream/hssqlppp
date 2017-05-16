
Pretty temporary at the moment


> {-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Tests.Parsing.QueryHints (queryHints) where
>
> --import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

> queryHints :: Item
> queryHints =
>    Group "queryHints"
>    [q "select a from tbl option (partition group);"
>       $ stbl {selOption = [QueryHintPartitionGroup]}
>    ,q "select a from tbl option (partition group,columnar host group);"
>       $ stbl {selOption = [QueryHintPartitionGroup,QueryHintColumnarHostGroup]}

>    ,q "select a from tbl option (set use_my_join = true);"
>       $ stbl {selOption = [QueryHintSet "use_my_join" $ SetId ea "true"]}

>    ,q "select a from tbl option (set use_my_join = true, partition group);"
>       $ stbl {selOption = [QueryHintSet "use_my_join" $ SetId ea "true",QueryHintPartitionGroup]}


>    ]
>    where
>      stbl = makeSelect
>             {selSelectList = sl [si $ ei "a"]
>             ,selTref = [tref "tbl"]}
>      q = QueryExpr
