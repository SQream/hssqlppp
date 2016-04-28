
> import System.Environment

> --import Language.Haskell.Exts
> --import Data.Generics.Uniplate.Data

> import Database.HsSqlPpp.Parse
> import GroomUtils
> import Text.Show.Pretty
> import qualified Data.Text.Lazy.IO as LT
> import Database.HsSqlPpp.Dialect

> main :: IO ()
> main = do
>   [f] <- getArgs
>   src <- LT.readFile f
>   let ast = parseStatements defaultParseFlags
>                {pfDialect = sqlServerDialect} f Nothing src
>   putStrLn $ either ppShow groomNoAnns ast
