module UI.Audio
(
    mainClickAudio,
) where
    import Control.Monad
    import Control.Monad.Fix

    import Data.List

    import Foreign.C.Types

    import System.Directory
    import System.Random

    import UI.Audio.Primitive


    mainClickAudio :: IO()
    mainClickAudio = select =<< map ps <$> files
        where
            ps :: FilePath -> IO()
            ps fp = loadAudio fp >>= playAudio 1

            select :: [IO()] -> IO()
            select fps = randomRIO (0,length fps-1) >>= (fps!!)

            files      :: IO [FilePath]
            files     = map ("click/main/"++) . filter (isInfixOf "wav")
                            <$> getDirectoryContents ("res/audio/click/main/")
