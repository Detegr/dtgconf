import Config
import Control.Monad.Reader

printItem (k,v) =
  case v of
    Just v  -> putStrLn $ "\t" ++ k ++ "=" ++ v
    Nothing -> putStrLn $ "\t" ++ k ++ "=" ++ "NO VALUE"

printSection (ConfigSection name items) = do
  putStrLn $ "[" ++ name ++ "]"
  mapM_ printItem items

printKeysFromSection (ConfigSection _ items) =
  mapM_ (\(k,_) -> putStrLn $ "\t" ++ k) items

main = withLoadedConfig "../example.conf" $ do
  sections <- getConfig

  -- Go through the whole config file
  liftIO $ mapM_ printSection sections

  snd <- getSection "Second Section"
  liftIO $
    case snd of
      Just s -> do
          putStrLn $ "Keys from " ++ sectionName s ++ ":"
          printKeysFromSection s
      Nothing -> putStrLn $ "Section not found"
