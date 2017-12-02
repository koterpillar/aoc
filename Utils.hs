module Utils where

getMultiline :: IO String
getMultiline = do
  ln <- getLine
  case ln of
    "" -> pure ""
    _ -> do
      rest <- getMultiline
      pure $ ln ++ "\n" ++ rest
