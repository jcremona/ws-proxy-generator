import System.Environment
import Model.Translate


main = do args <- getArgs
          case args of
             [file] -> wsdlToModules file
             _ -> putStrLn "Error: unknown args"
