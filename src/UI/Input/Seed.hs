module UI.Input.Seed
(
    getSeed,
) where

    import Data.Char

    import System.Random


    getSeed :: IO Int
    getSeed = putStrLn "Please input number to seed (blank for random):"
           >> trim <$> getLine >>= testSeed

    testSeed :: String -> IO Int
    testSeed str
        | null str       = abs <$> randomIO
        | onlyDigits str = return $ read str
        | otherwise      = getSeed


    trim :: String -> String
    trim = let f = reverse . dropWhile isSpace in f . f

    onlyDigits :: String -> Bool
    onlyDigits (c:cs) = foldr ((&&) . isDigit) True cs
