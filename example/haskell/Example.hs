import Config
import Control.Monad.Reader
import System.Random(randomIO)

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

  snd <- getSection "Second section"
  liftIO $
    case snd of
      Just s -> do
          putStrLn $ "Keys from " ++ sectionName s ++ ":"
          printKeysFromSection s
      Nothing -> putStrLn $ "Section not found"

  -- If a section is known where the key exists, it can be specified to make the search more efficient
  i <- getItem "Key2" Nothing
  liftIO $ case i of
    Just i  -> putStrLn "Item: " >> printItem i
    Nothing -> putStrLn $ "Item not found"
  
  -- Add/update random value to the config
  rnd <- liftIO $ (randomIO :: IO Int)
  addItem "First section" "Random value from example" (show rnd)

  saveConfig
