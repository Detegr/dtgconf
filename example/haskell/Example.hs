import Config

main = withLoadedConfig "../example.conf" $ \c -> do
  mapM (putStrLn . show) =<< getConfig c
