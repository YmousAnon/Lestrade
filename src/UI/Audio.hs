module UI.Audio
(
    mainClickAudio,
    secondaryClickAudio,
) where
    import Control.Monad
    import Control.Monad.Fix

    import Data.List

    import Foreign.C.Types

    import System.Directory
    import System.IO.Unsafe
    import System.Random

    import UI.Audio.Primitive


    mainClickAudio :: IO()
    mainClickAudio = (select =<< map ps <$> files)
        where
            ps :: FilePath -> IO()
            ps fp = loadAudio fp >>= playAudio 1

            select :: [IO()] -> IO()
            select as = randomRIO (0,length as-1) >>= (as!!)

            files :: IO [FilePath]
            files = map ("click/main/"++) . filter (isInfixOf "wav")
                        <$> getDirectoryContents ("res/audio/click/main/")


    secondaryClickAudio :: Int -> [IO()]
    secondaryClickAudio i = (select =<< map ps <$> files)
                          : secondaryClickAudio (i+1)
        where
            ps :: FilePath -> IO()
            ps fp = loadAudio fp >>= playAudio 1

            select :: [IO()] -> IO()
            select as = as!!mod i (length as)

            files :: IO[FilePath]
            files = map ("click/secondary/"++) . filter (isInfixOf "wav")
                        <$> getDirectoryContents ("res/audio/click/secondary/")

