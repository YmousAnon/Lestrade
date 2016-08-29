module UI.Audio.Primitive
(
    Audio,

    loadAudio,
    playAudio,
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



    globalVolume :: IO CFloat
    globalVolume = read <$> getSetting "volume"
