module GraphicsClient (runGraphicsClient) where


runGraphicsClient :: String -> IO ()
runGraphicsClient username = do
  putStrLn username
