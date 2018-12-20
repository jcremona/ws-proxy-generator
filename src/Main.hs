import System.Environment
import Model.Translate
import Network.HTTP

main = do args <- getArgs
          case args of
             [file] -> wsdlToModules file
             _ -> putStrLn "Error: unknown args"

writeModuleList ms = mapM (\module_ -> write module_ >> putStrLn ("Module Generated: " ++ (moduleName module_))) ms
--             
writeModuleLists mss = do mapM writeModuleList mss
                          return ()

wsdlToModules path = do response <- simpleHTTP $ getRequest path
                        case (fmap rspBody response) of
                             Left err -> putStrLn . show $ err
                             Right wsdlFile -> either (putStrLn . show) writeModuleLists (translate wsdlFile)
