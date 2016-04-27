
> import Database.HsSqlPpp.Lex
> import Text.Show.Pretty
> import System.Environment
> import qualified Data.Text.IO as T

> main :: IO ()
> main = do
>   [s] <- getArgs
>   f <- T.readFile s
>   putStrLn $ ppShow $ lexTokens PostgreSQLDialect "" (Just (1,0)) f
>   --putStrLn $ ppShow $ parsePlpgsql "" s
