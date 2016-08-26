module UI.Input.Settings
(
    getSetting,
) where

    import Data.Char
    import Data.List.Split
    import Data.Map (fromList, Map, lookup)
    import Data.Maybe

    import System.IO


    getSetting :: String -> IO String
    getSetting key = Data.Map.lookup key <$> rc >>= \mVal ->
        case mVal of
            Just val -> return val
            Nothing  -> putStrLn error >> return ""
        where
            rc :: IO(Map String String)
            rc = fromList
               . map toTouples
               . filter (not . Prelude.null)
               . map removeComments
               . toLines ""
             <$> readFile file
                where
                    file :: FilePath
                    file = "lestraderc.hs"

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
            toLines s  [c]      = [s]
            toLines s  []       = [s]
            toLines "" (' ':cs) = toLines "" cs
            toLines s  (c:cs)
                | c == '\n'           = s : toLines ""       cs
                | c == '\n'           = s : toLines ""       cs
                | otherwise           =     toLines (s++[c]) cs
