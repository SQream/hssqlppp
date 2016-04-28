
> import System.Environment
> import Data.List

> import Text.Show.Pretty

> import Database.HsSqlPpp.Parse
> import Database.HsSqlPpp.TypeCheck
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Dialect
> import qualified Data.Text.Lazy.IO as LT

> main :: IO ()
> main = do
>   [f] <- getArgs
>   src <- LT.readFile f
>   let ast = either (error . show ) id
>             $ parseStatements defaultParseFlags f Nothing src
>       catIn = diDefaultCatalog $ tcfDialect defaultTypeCheckingFlags
>   let (cat,_) =
>                 typeCheckStatements defaultTypeCheckingFlags
>                 (diDefaultCatalog $ tcfDialect defaultTypeCheckingFlags) ast
>       cc = deconstructCatalog cat \\ deconstructCatalog catIn
>   putStrLn $ ppShow cc
