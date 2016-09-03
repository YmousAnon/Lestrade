module UI.Input.Settings
(
    getSetting,
) where

    import Data.Char
    import Data.List.Split
    import Data.Map (fromList, Map, lookup)
    import Data.Maybe

    import System.IO
    import System.IO.Unsafe


    rc :: Map String String
    rc = unsafePerformIO
       $ fromList
       . map toTouples
       . filter (not . Prelude.null)
       . map removeComments
       . toLines ""
       <$> readFile file
        where
            file :: FilePath
            file = "lestraderc.hs"

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

    getSetting :: String -> IO String
    getSetting key =  (\mVal ->
        case mVal of
            Just val -> return val
            Nothing  -> return ""
            ) (Data.Map.lookup key rc)
        where
            error :: String
            error = "Fatal error, option "++key++" not specified in \
                    \configuration file."
