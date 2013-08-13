import Config

main = withLoadedConfig "../example.conf" $ \c -> do
  conf <- getConfig c
  mapM (putStrLn . show) conf
