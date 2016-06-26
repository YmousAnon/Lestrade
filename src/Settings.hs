module Settings
(
    getSetting,
) where

    import System.IO
    import Data.List.Split
    import Data.Char
    import Data.Map
    import Data.Maybe
    --import Data.ConfigFile
    --import Data.Either.Utils


    getSetting :: String -> IO String
    getSetting key = (Data.Map.lookup key) <$> rc >>= \mVal ->
        case mVal of
            Just val -> return val
            Nothing  -> putStrLn error >> return ""
        where
            rc :: IO(Map String String)
            rc = fromList
             <$> Prelude.map toTouples
             <$> Prelude.filter (\s -> length s > 0)
             <$> Prelude.map removeComments
             <$> toLines ""
             <$> readFile file
                where
                    file :: FilePath
                    file = "sherlockrc.hs"

            error :: String
            error = "Fatal error, option "++key++" not specified in \
                    \configuration file."

            toTouples :: String -> (String,String)
            toTouples s = (\[k,v] -> (k,v)) $ Prelude.map trim $ splitOn "=" s
                where trim = let f = reverse . dropWhile isSpace in f . f

            removeComments :: String -> String
            removeComments []           = ""
            removeComments ('-':'-':cs) = ""
            removeComments (c:cs)       = c : removeComments cs

            toLines :: String -> String -> [String]
            toLines s  (c:[])   = [s]
            toLines s  []       = [s]
            toLines "" (' ':cs) = toLines "" cs
            toLines s  (c:cs)
                | c == '\n'           = s : toLines ""       cs
                | c == '\n'           = s : toLines ""       cs
                | otherwise           =     toLines (s++[c]) cs

