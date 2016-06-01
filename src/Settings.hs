module Settings
(
    getVal
) where

    import Data.ConfigFile
    import Data.Either.Utils


    cp :: IO ConfigParser
    cp = forceEither <$> readfile emptyCP "sherlockrc"


    getVal :: String -> IO String
    getVal key = (\cp' -> forceEither $ get cp' "DEFAULT" key) <$> cp
