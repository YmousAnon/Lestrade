module Settings
(
    getVal
) where

    import Data.ConfigFile
    import Data.Either.Utils


    cp :: IO ConfigParser
    cp = forceEither <$> readfile emptyCP "sherlockrc"


    getVal :: String -> String -> IO String
    --getVal :: Read a => String -> String -> IO a
    getVal sect key = (\cp' -> forceEither $ get cp' sect key) <$> cp
