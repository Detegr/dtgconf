import Config
import Control.Monad.Reader

{-main = withLoadedConfig "../example.conf" $ \c -> do
  mapM (putStrLn . show) =<< getConfig c-}

printItem (k,v) =
  case v of
    Just v  -> putStrLn $ k ++ "=" ++ v
    Nothing -> putStrLn $ k ++ "=" ++ "NO VALUE"

printSection EmptySection = return ()
printSection (ConfigSection name items) = do
  putStrLn $ "[" ++ name ++ "]"
  mapM_ printItem items

printKeysFromSection EmptySection = return ()
printKeysFromSection (ConfigSection _ items) = mapM_ (\(k,_) -> putStrLn k) items

main = withLoadedConfig "../example.conf" $ do
  sections <- getConfig

  -- Go through the whole config file
  liftIO $ mapM_ printSection sections

  snd <- getSection "Second Section"
  liftIO $ do
    putStrLn $ "Keys from " ++ sectionName snd ++ ":"
    printKeysFromSection snd
