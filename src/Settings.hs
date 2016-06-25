module Settings
(
    getVal
) where

    import System.IO
    import Data.List.Split
    import Data.Char
    import Data.Map
    import Data.Maybe
    --import Data.ConfigFile
    --import Data.Either.Utils


    getVal :: String -> IO String
    --getVal :: Read a => String -> String -> IO a
    getVal key = fromJust <$> (Data.Map.lookup key) <$> rc

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

