module UI.Audio.Primitive
(
    Audio,

    loadAudio,
    playAudio,
    loadAndPlayAudio,
) where

    import Control.Monad
    import Control.Monad.Fix

    import Foreign.C.Types

    import Sound.ALUT

    import UI.Input.Settings


    type Audio = Source

    loadAudio :: FilePath -> IO Audio
    loadAudio file = do buf    <- createBuffer $ File ("res/audio/"++file)
                        source <- genObjectName
                        buffer source $= Just buf
                        return source

    playAudio :: CFloat -> Audio -> IO()
    playAudio v s = globalVolume >>= (sourceGain s $=) . (v*) >> play [s]

    loadAndPlayAudio :: CFloat -> FilePath -> IO()
    loadAndPlayAudio v fp = loadAudio fp >>= playAudio v


    globalVolume :: IO CFloat
    globalVolume = read <$> getSetting "volume"
