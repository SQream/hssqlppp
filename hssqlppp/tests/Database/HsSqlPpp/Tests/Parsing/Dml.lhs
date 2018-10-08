
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.Dml (dml) where
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

TODO:

from in update, using in delete (+ type check these)

> dml:: Item
> dml =
>    Group "dml" [
>      Group "insert" [
>       s "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2);\n"
>        [Insert ea
>         (name "testtable")
>         [Nmc "columna", Nmc "columnb"]
>         (Values ea [[num "1", num "2"]])
>         Nothing]

multi row insert, test the stand alone values statement first, maybe
that should be in the select section?

>      ,s "values (1,2), (3,4);"
>      [QueryStatement ea $ Values ea [[num "1", num "2"]
>                                     ,[num "3", num "4"]]]
>
>      ,s "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2), (3,4);\n"
>       [Insert ea
>         (name "testtable")
>         [Nmc "columna", Nmc "columnb"]
>         (Values ea [[num "1", num "2"]
>                    ,[num "3", num "4"]])
>         Nothing]

insert from select

>      ,s "insert into a\n\
>          \    select b from c;"
>       [Insert ea (name "a") []
>        (makeSelect
>         {selSelectList = sl [si $ ei "b"]
>         ,selTref = [tref "c"]})
>        Nothing]
>
>      ,s "insert into testtable\n\
>         \(columna,columnb)\n\
>         \values (1,2) returning id;\n"
>       [Insert ea
>         (name "testtable")
>         [Nmc "columna", Nmc "columnb"]
>         (Values ea [[num "1", num "2"]])
>         (Just $ sl [si $ ei "id"])]
>      ]
>
>     ,Group "update" [
>       s "update tb\n\
>         \  set x = 1, y = 2;"
>       [Update ea (name "tb") [set "x" $ num "1"
>                             ,set "y" $ num "2"]
>        [] Nothing Nothing]
>      ,s "update tb\n\
>         \  set x = 1, y = 2 where z = true;"
>       [Update ea (name "tb") [set "x" $ num "1"
>                             ,set "y" $ num "2"]
>        []
>        (Just $ binop "=" (ei "z") lTrue)
>        Nothing]
>      ,s "update tb\n\
>         \  set x = 1, y = 2 returning id;"
>       [Update ea (name "tb") [set "x" $ num "1"
>                             ,set "y" $ num "2"]
>        [] Nothing (Just $ sl [si $ ei "id"])]
>      ,s "update tb\n\
>         \  set (x,y) = (1,2);"
>       [Update ea (name "tb")
>        [MultiSetClause ea [Nmc "x",Nmc "y"]
>         $ specop "rowctor" [num "1"
>                             ,num "2"]]
>        []
>        Nothing Nothing]
>      ]

App ea "=" [App ea "rowctor" [Identifier ea "x",Identifier ea "y"],App ea "rowctor" [num "1",num "2"]])


>
>     ,Group "delete" [
>       s "delete from tbl1 where x = true;"
>       [Delete ea (name "tbl1") [] (Just $ binop "=" (ei "x") lTrue)
>        Nothing]
>      ,s "delete from tbl1 where x = true returning id;"
>       [Delete ea (name "tbl1") [] (Just $ binop "=" (ei "x") lTrue)
>        (Just $ sl [si $ ei "id"])]
>      ]
>
>     ,Group "truncate" [
>       s "truncate test;"
>        [Truncate ea [name "test"] ContinueIdentity Restrict]
>
>      ,s "truncate table test, test2 restart identity cascade;"
>        [Truncate ea [name "test",name "test2"] RestartIdentity Cascade]
>      ]

copy, bit crap at the moment

>     ,Group "copy" [

-- old copy from options

        s "copy tbl(a,b) from stdin;\n\
          \bat\tt\n\
          \bear\tf\n\
          \\\.\n"
        [ CopyFrom ea (name "tbl")
          [ Nmc "a", Nmc "b" ]
          Stdin
          ( OldCopyFromOptions
            [
            ]
          )
        , CopyData ea "\
          \bat\tt\n\
          \bear\tf\n"
        ]

>        s "copy tbl (a,b) from 'filename' with delimiter '|';"
>        [ CopyFrom ea (name "tbl")
>          [ Nmc "a", Nmc "b" ]
>          ( CopyFilename "filename")
>          ( OldCopyFromOptions
>            [ CopyFromDelimiter "|"
>            ]
>          )
>        ]
>      , s "copy tbl (a,b) from 'filename' with delimiter '|' parsers 'ISO8601';"
>        [ CopyFrom ea (name "tbl")
>          [ Nmc "a", Nmc "b" ]
>          ( CopyFilename "filename" )
>          ( OldCopyFromOptions
>            [ CopyFromDelimiter "|"
>            , CopyFromParsers "ISO8601"
>            ]
>          )
>        ]
>      , s "copy tbl (a,b) from 'filename' with delimiter '|' error_log 'errors.log';"
>        [ CopyFrom ea (name "tbl")
>          [ Nmc "a", Nmc "b" ]
>          ( CopyFilename "filename" )
>          ( OldCopyFromOptions
>            [ CopyFromDelimiter "|"
>            , CopyFromErrorLog "errors.log"
>            ]
>          )
>        ]
>      , s "copy tbl (a,b) from 'filename' with delimiter '|' error_log 'errors.log' error_verbosity 1;"
>        [ CopyFrom ea (name "tbl")
>          [ Nmc "a", Nmc "b" ]
>          ( CopyFilename "filename" )
>          ( OldCopyFromOptions
>            [ CopyFromDelimiter "|"
>            , CopyFromErrorLog "errors.log"
>            , CopyFromErrorVerbosity 1
>            ]
>          )
>        ]
>      , s "copy tbl (a,b) from 'filename' with delimiter '|' error_log 'errors.log' error_verbosity 1 parsers 'ISO8601';"
>        [ CopyFrom ea (name "tbl")
>          [ Nmc "a", Nmc "b" ]
>          ( CopyFilename "filename" )
>          ( OldCopyFromOptions
>            [ CopyFromDelimiter "|"
>            , CopyFromErrorLog "errors.log"
>            , CopyFromErrorVerbosity 1
>            , CopyFromParsers "ISO8601"
>            ]
>          )
>        ]
>      , s "copy tbl (a,b) from 'filename' with delimiter E'\\001';"
>        [ CopyFrom ea (name "tbl")
>          [ Nmc "a", Nmc "b" ]
>          ( CopyFilename "filename" )
>          ( OldCopyFromOptions
>            [ CopyFromOctalDelimiter 1
>            ]
>          )
>        ]
>      , s "copy tbl (a,b) from 'filename' with delimiter E'\\111' error_log 'errors.log' error_verbosity 1 parsers 'ISO8601';"
>        [ CopyFrom ea (name "tbl")
>          [ Nmc "a", Nmc "b" ]
>          ( CopyFilename "filename" )
>          ( OldCopyFromOptions
>            [ CopyFromOctalDelimiter 73
>            , CopyFromErrorLog "errors.log"
>            , CopyFromErrorVerbosity 1
>            , CopyFromParsers "ISO8601"
>            ]
>          )
>        ]
>      , s "copy tbl (a,b) from 'filename' with error_log 'errors.log' error_verbosity 1 delimiter E'\\111' parsers 'ISO8601';"
>        [ CopyFrom ea (name "tbl")
>          [ Nmc "a", Nmc "b" ]
>          ( CopyFilename "filename" )
>          ( OldCopyFromOptions
>            [ CopyFromOctalDelimiter 73
>            , CopyFromErrorLog "errors.log"
>            , CopyFromErrorVerbosity 1
>            , CopyFromParsers "ISO8601"
>            ]
>          )
>        ]

-- new copy from options

>      , s "copy tbl (a,b) from 'filepath' with options field delimiter '|' text qualifier '?' null marker 'null' on error abort offset 1 limit 10 date format 'ISO8601';"
>        [ CopyFrom ea (name "tbl")
>          [ Nmc "a", Nmc "b" ]
>          ( CopyFilename "filepath" )
>          ( NewCopyFromOptions $ NewCsvOptions
>            { csvFilePath = "filepath"
>            , csvFieldDelimiter = Just (StringDelimiter "|")
>            , csvRecordDelimiter = Nothing
>            , csvTextQualifier = Just (StringDelimiter "?")
>            , csvNullMarker = Just "null"
>            , csvErrorOptions = Just EOAbort
>            , csvLimit = Just 10
>            , csvOffset = Just 1
>            , csvDateFormat = Just "ISO8601"
>            }
>          )
>        ]
>      , s "copy tbl (a,b) from 'filepath' with options field delimiter '|' text qualifier '?' null marker 'null' on error skip row max 100 offset 1 limit 10 date format 'ISO8601';"
>        [ CopyFrom ea (name "tbl")
>          [ Nmc "a", Nmc "b" ]
>          ( CopyFilename "filepath" )
>          ( NewCopyFromOptions $ NewCsvOptions
>            { csvFilePath = "filepath"
>            , csvFieldDelimiter = Just (StringDelimiter "|")
>            , csvRecordDelimiter = Nothing
>            , csvTextQualifier = Just (StringDelimiter "?")
>            , csvNullMarker = Just "null"
>            , csvErrorOptions = Just (EOSkipRow (RowMaxNum 100) NoReportSkippedRows)
>            , csvLimit = Just 10
>            , csvOffset = Just 1
>            , csvDateFormat = Just "ISO8601"
>            }
>          )
>        ]
>      , s "copy tbl (a,b) from 'filepath' with options field delimiter '|' text qualifier '?' null marker 'null' on error skip row offset 1 limit 10 date format 'ISO8601';"
>        [ CopyFrom ea (name "tbl")
>          [ Nmc "a", Nmc "b" ]
>          ( CopyFilename "filepath" )
>          ( NewCopyFromOptions $ NewCsvOptions
>            { csvFilePath = "filepath"
>            , csvFieldDelimiter = Just (StringDelimiter "|")
>            , csvRecordDelimiter = Nothing
>            , csvTextQualifier = Just (StringDelimiter "?")
>            , csvNullMarker = Just "null"
>            , csvErrorOptions = Just (EOSkipRow NoRowMax NoReportSkippedRows)
>            , csvLimit = Just 10
>            , csvOffset = Just 1
>            , csvDateFormat = Just "ISO8601"
>            }
>          )
>        ]
>      , s "copy tbl to 'file';"
>        [ CopyTo ea (CopyTable (name "tbl") []) "file" []]
>      ,s "copy tbl(a,b) to 'file';"
>       [CopyTo ea (CopyTable (name "tbl") [Nmc "a", Nmc "b"]) "file" []]
>      ,s "copy (select * from tbl) to 'file' with format binary;"
>       [CopyTo ea (CopyQuery $ makeSelect {selSelectList = sl [si $ Star ea]
>                                          ,selTref = [tref "tbl"]})
>        "file" [CopyToFormat "binary"]]
>      ,s "copy (select * from tbl) to 'file' with format binary delimiter E'\\010';"
>       [CopyTo ea (CopyQuery $ makeSelect {selSelectList = sl [si $ Star ea]
>                                          ,selTref = [tref "tbl"]})
>        "file" [CopyToFormat "binary", CopyToOctalDelimiter 8]]
>      ]
>      ,s "copy tbl to 'file' with header;"
>       [CopyTo ea (CopyTable (name "tbl") []) "file" [CopyToHeader]]
>    ]
>  where
>    s = Stmt
